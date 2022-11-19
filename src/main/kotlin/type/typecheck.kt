package org.altk.lab.mxc.type

import org.altk.lab.mxc.MxcInternalError
import org.altk.lab.mxc.ReferenceError
import org.altk.lab.mxc.SyntaxError
import org.altk.lab.mxc.TypeError
import org.altk.lab.mxc.ast.*
import org.altk.lab.mxc.ast.Node as AstNode

fun typecheck(ast: Program) = TypecheckRecord(ast)

open class TypecheckRecord(val ast: Program) {
  val globalEnv = createGlobalEnv()
  val envs = HashMap<AstNode, EnvironmentRecord>()
  val types = HashMap<Expression, Type>()
  val structs = HashMap<String, MxStruct>()
  val hasNormalCompletion = HashMap<AstNode, Boolean>()
  val references = HashMap<Identifier, ReferenceRecord>()

  private val returnTypes = HashMap<Statement, Type>()
  private val assignable = HashMap<LeftHandSideExpression, Boolean>()

  init {
    envs[ast] = globalEnv

    collectTypes()

    collectClassFields()
    collectFunctionSignatures()

    collectGlobals()
  }

  protected open val AstNode.env get() = envs[this]
  protected val Expression.currentType get() = types[this] ?: MxHole
  protected val Statement.currentReturnType get() = returnTypes[this] ?: MxBot
  protected val AstNode.hasNormalCompletion
    get() = this@TypecheckRecord.hasNormalCompletion[this]
  protected val LeftHandSideExpression.assignable
    get() = this@TypecheckRecord.assignable[this]

  private fun LeftHandSideExpression.checkAssignable() {
    if (!assignable!!) {
      val msg = "Left-hand-side expression is not assignable"
      throw ReferenceError(ctx, msg)
    }
  }

  private fun Expression.checkNotBot() = currentType.checkNotBot(ctx)
  private fun Expression.checkCompleteness() =
    currentType.checkCompleteness(ctx)

  protected val classes get() = ast.body.filterIsInstance<ClassDeclaration>()
  protected val functions get() = ast.body.filterIsInstance<FunctionDeclaration>()

  private fun collectTypes() {
    for (class_ in classes) {
      val name = class_.id.name
      val env = ClassEnvironmentRecord(globalEnv)
      envs[class_] = env
      val type = MxStruct(name, env)
      env.type = type
      structs[name] = type
      val binding = TypeBinding(class_.ctx, name, type)
      globalEnv.createBinding(binding)
    }
  }

  private fun collectClassFields() {
    for (class_ in classes) {
      val env = class_.env!!

      for (decl in class_.body.filterIsInstance<VariableDeclaration>()) {
        val type = decl.typeId.resolve(env)
        if (!type.complete) {
          throw TypeError(decl.ctx, "Class field has incomplete type $type")
        }
        for (v in decl.declarations) {
          if (v.init != null) {
            throw SyntaxError(v.ctx, "Class fields cannot have initializers")
          }
          env.createBinding(Binding(v.ctx, v.id.name, type, Mutability.MUTABLE))
        }
      }

      for (decl in class_.body.filterIsInstance<FunctionDeclaration>()) {
        val binding =
          Binding(decl.ctx, decl.id.name, decl.type(env), Mutability.IMMUTABLE)
        env.createBinding(binding)
      }
    }
  }

  private fun collectFunctionSignatures() {
    for (fn in functions) {
      val binding =
        Binding(fn.ctx, fn.id.name, fn.type(globalEnv), Mutability.IMMUTABLE)
      globalEnv.createBinding(binding)
    }
  }

  private fun collectGlobals() {
    for (node in ast.body) when (node) {
      is VariableDeclaration -> infer(node, globalEnv)
      is FunctionDeclaration -> collectFunctionDeclaration(node, globalEnv)
      is ClassDeclaration -> {
        val classEnv = node.env!!
        if (node.body.count { it is ConstructorDeclaration } > 1) {
          throw ReferenceError(
            node.ctx,
            "Class contains more than one constructor",
          )
        }
        for (decl in node.body) when (decl) {
          is VariableDeclaration -> Unit
          is FunctionDeclaration -> collectFunctionDeclaration(decl, classEnv)
          is ConstructorDeclaration -> {
            if (decl.id.name != node.id.name) {
              val msg =
                "Constructor name mismatch (expected ${node.id.name}, got ${decl.id.name})"
              throw ReferenceError(node.ctx, msg)
            }
            infer(decl.body, classEnv)
            check(decl.body, MxVoid)
          }
        }
      }
    }
  }

  private fun collectFunctionDeclaration(
    node: FunctionDeclaration,
    env: EnvironmentRecord,
  ) {
    val functionEnv = FunctionEnvironmentRecord(env)
    envs[node] = functionEnv
    val params = node.params.map {
      val ty = it.typeId.resolve(functionEnv)
      val binding = Binding(it.ctx, it.id.name, ty, Mutability.MUTABLE)
      functionEnv.createBinding(binding)
      references[it.id] = ReferenceRecord(functionEnv, binding)
      ty
    }
    infer(node.body, functionEnv)
    val declaredReturnType = node.returnType.resolve(env)
    val actualReturnType = node.body.currentReturnType union
      if (node.body.hasNormalCompletion!!) MxVoid else MxBot
    if (actualReturnType is MxTop) {
      throw TypeError(node.ctx, "Conflicting function return types")
    }
    val returnType = actualReturnType intersect declaredReturnType
    if (returnType is MxBot) {
      val msg =
        "Function return type mismatch (expected $declaredReturnType, got $actualReturnType)"
      throw TypeError(node.ctx, msg)
    }
    returnType.checkCompleteness(node.ctx)
    val ty = MxFunction(params, returnType)
    val binding = Binding(node.ctx, node.id.name, ty, Mutability.IMMUTABLE)
    env.setBinding(binding)
  }

  private fun infer(node: Statement, env: EnvironmentRecord) {
    envs[node] = env
    hasNormalCompletion[node] = true
    when (node) {
      is BlockStatement -> {
        val blockEnv = EnvironmentRecord(env)
        envs[node] = blockEnv
        for (stmt in node.body) infer(stmt, blockEnv)
        hasNormalCompletion[node] = node.body
          .map { it.hasNormalCompletion!! }
          .fold(true) { a, b -> a && b }
        returnTypes[node] = node.body
          .map { it.currentReturnType }
          .fold(MxHole as Type) { a, b -> a union b }
      }

      is BreakStatement, is ContinueStatement -> {
        if (env.loopEnv == null) {
          throw SyntaxError(node.ctx, "loop statement not in loop")
        }
        hasNormalCompletion[node] = false
      }

      is EmptyStatement -> {}
      is ExpressionStatement -> {
        infer(node.expression, env)
        if (node.expression.currentType != MxNullptr) {
          node.expression.checkCompleteness()
        }
      }

      is IfStatement -> {
        envs[node] = env
        infer(node.test, env)
        check(node.test, MxBool)

        infer(node.consequent, EnvironmentRecord(env))
        node.alternate?.let { infer(it, EnvironmentRecord(env)) }
        hasNormalCompletion[node] =
          node.test.hasNormalCompletion!! &&
            (node.consequent.hasNormalCompletion!! || node.alternate?.hasNormalCompletion ?: true)
        returnTypes[node] = if (node.alternate == null) {
          node.consequent.currentReturnType
        } else {
          node.consequent.currentReturnType union node.alternate.currentReturnType
        }
      }

      is ForStatement -> {
        val forEnv = LoopEnvironmentRecord(env)
        envs[node] = forEnv
        when (node.init) {
          null -> Unit
          is VariableDeclaration -> infer(node.init, forEnv)
          is Expression -> {
            infer(node.init, forEnv)
            node.init.checkCompleteness()
          }
        }
        node.test?.let { infer(it, forEnv); check(it, MxBool) }
        node.update?.let { infer(it, forEnv); it.checkCompleteness() }
        infer(node.body, forEnv)
        // TODO: finer check when test is null and there are no breaks
        hasNormalCompletion[node] = when (node.init) {
          null -> true
          is Expression -> node.init.hasNormalCompletion!!
          is VariableDeclaration -> node.init.hasNormalCompletion!!
        } && node.test?.hasNormalCompletion ?: true
        returnTypes[node] = node.body.currentReturnType
      }

      is WhileStatement -> {
        val whileEnv = LoopEnvironmentRecord(env)
        envs[node] = whileEnv
        infer(node.test, whileEnv)
        node.test.checkCompleteness()
        infer(node.body, whileEnv)
        hasNormalCompletion[node] = node.test.hasNormalCompletion!!
        returnTypes[node] = node.body.currentReturnType
      }

      is ReturnStatement -> {
        hasNormalCompletion[node] = false
        returnTypes[node] = if (node.argument != null) {
          infer(node.argument, env)
          node.argument.currentType
        } else {
          MxVoid
        }
      }

      is VariableDeclaration -> {
        returnTypes[node] = MxBot
        val declaredType = node.typeId.resolve(env)
        for (decl in node.declarations) decl.init?.let { infer(it, env) }
        val ty = node.declarations
          .map { it.init?.currentType ?: MxTop }
          .fold(declaredType) { a, b -> a intersect b }
        ty.checkCompleteness(node.ctx)
        if (ty.isVoid) {
          throw TypeError(node.ctx, "Variable has incomplete type $ty")
        }
        for (decl in node.declarations) decl.init?.let { check(it, ty) }
        for (decl in node.declarations) {
          val binding = Binding(decl.ctx, decl.id.name, ty, Mutability.MUTABLE)
          env.createBinding(binding)
          references[decl.id] = ReferenceRecord(env, binding)
        }
      }
    }
  }

  private fun check(node: Statement, returnType: Type): Unit = when (node) {
    is BreakStatement, is ContinueStatement, is EmptyStatement,
    is ExpressionStatement, is VariableDeclaration -> Unit

    is BlockStatement -> node.body.forEach { check(it, returnType) }

    is ForStatement -> check(node.body, returnType)
    is WhileStatement -> check(node.body, returnType)
    is IfStatement -> {
      node.alternate?.let { check(it, returnType) }
      check(node.consequent, returnType)
    }

    is ReturnStatement -> {
      if (node.argument == null) {
        if (returnType != MxVoid) {
          val msg = "Return statement expecting $returnType, got void"
          throw TypeError(node.ctx, msg)
        }
      } else {
        check(node.argument, returnType)
      }
      @Suppress Unit
    }
  }

  private fun infer(node: Expression, env: EnvironmentRecord) {
    envs[node] = env
    hasNormalCompletion[node] = true
    types[node] = when (node) {
      is AssignmentExpression -> {
        infer(node.left, env)
        infer(node.right, env)
        node.left.checkAssignable()
        val ty = node.left.currentType intersect node.right.currentType
        ty.checkCompleteness(node.ctx)
        check(node.left, ty)
        check(node.right, ty)
        // TODO
        MxVoid
      }

      is BinaryExpression -> {
        infer(node.left, env)
        infer(node.right, env)
        val ty = node.left.currentType intersect node.right.currentType
        val valid = when (ty) {
          is MxArray ->
            node.operator.isEquivalenceOp &&
              (node.left.currentType is MxNullptr || node.right.currentType is MxNullptr)

          is MxStruct, MxNullptr -> node.operator.isEquivalenceOp
          else -> ty in node.operator.operandTypes
        }
        if (!valid) {
          throw TypeError(node.ctx, "Invalid operand type (got $ty)")
        }
        if (ty !is MxNullptr) ty.checkCompleteness(node.ctx)
        check(node.left, ty)
        check(node.right, ty)
        node.operator.resultType(ty)
      }

      is CallExpression -> {
        infer(node.callee, env)
        node.arguments.forEach { infer(it, env) }
        val calleeType = node.callee.currentType
        val ty = calleeType intersect
          MxFunction(node.arguments.map { it.currentType }, MxHole)
        if (calleeType is MxFunction && calleeType.params.size != node.arguments.size) {
          val msg =
            "Function call got wrong number of arguments (expected ${calleeType.params.size}, got ${node.arguments.size})"
          throw TypeError(node.ctx, msg)
        }
        ty.checkCompleteness(node.ctx)
        if (ty !is MxFunction) {
          val msg = "$ty is not a function"
          throw MxcInternalError(node.ctx, msg)
        }
        check(node.callee, ty)
        node.arguments.zip(ty.params).forEach { check(it.first, it.second) }
        ty.returnType
      }

      is GroupExpression -> {
        infer(node.expression, env)
        assignable[node] =
          (node.expression as? LeftHandSideExpression)?.assignable ?: false
        node.expression.currentType
      }

      is LambdaExpression -> {
        val lambdaEnv =
          FunctionEnvironmentRecord(if (node.capture) env else globalEnv)
        envs[node] = lambdaEnv
        val params = node.params.map {
          val ty = it.typeId.resolve(lambdaEnv)
          val binding = Binding(it.ctx, it.id.name, ty, Mutability.MUTABLE)
          lambdaEnv.createBinding(binding)
          ty
        }
        infer(node.body, lambdaEnv)
        MxFunction(params, node.body.currentReturnType)
      }

      is ComputedMemberExpression -> {
        assignable[node] = true

        infer(node.`object`, env)
        val objectType = node.`object`.currentType intersect MxArray(MxHole)
        if (objectType !is MxArray) {
          val msg = "Cannot read properties of ${node.`object`.currentType}"
          throw TypeError(node.ctx, msg)
        }
        check(node.`object`, objectType)

        infer(node.prop, env)
        val propType = node.prop.currentType intersect MxInt
        if (propType != MxInt) {
          val msg = "Cannot read property of type $propType (int expected)"
          throw TypeError(node.ctx, msg)
        }
        check(node.prop, propType)

        objectType.content
      }

      is MemberExpression -> {
        infer(node.`object`, env)
        val objectType = node.`object`.currentType
        if (objectType is MxHole) {
          throw TypeError(node.`object`.ctx, "Unable to deduce type")
        }
        if (objectType !is ObjectType) {
          throw TypeError(node.ctx, "Cannot read properties of $objectType")
        }
        check(node.`object`, objectType)

        val binding = objectType.env.getBinding(node.ctx, node.prop.name)
        assignable[node] = binding.mutability == Mutability.MUTABLE

        binding.type
      }

      is Identifier -> {
        val ref = env.getIdentifierReference(node.ctx, node.name)
          ?: throw ReferenceError(node.ctx, "${node.name} is not defined")
        references[node] = ref
        assignable[node] = ref.binding.mutability == Mutability.MUTABLE
        ref.binding.type
      }

      is PrefixUpdateExpression -> {
        infer(node.argument, env)
        node.argument.checkAssignable()
        val ty = node.argument.currentType intersect MxInt
        if (ty != MxInt) {
          throw TypeError(node.ctx, "Invalid operand type $ty (int expected)")
        }
        check(node.argument, ty)
        assignable[node] = true
        ty
      }

      is PostfixUpdateExpression -> {
        infer(node.argument, env)
        node.argument.checkAssignable()
        val ty = node.argument.currentType intersect MxInt
        if (ty != MxInt) {
          throw TypeError(node.ctx, "Invalid operand type $ty (int expected)")
        }
        check(node.argument, ty)
        ty
      }

      is UnaryExpression -> {
        infer(node.argument, env)
        val expected = node.operator.operandType
        val ty = node.argument.currentType intersect expected
        if (ty != expected) {
          val msg = "Invalid operand type $ty ($expected expected)"
          throw TypeError(node.ctx, msg)
        }
        check(node.argument, ty)
        ty
      }

      is BooleanLiteral -> MxBool
      is IntegerLiteral -> MxInt
      is NullLiteral -> MxNullptr
      is StringLiteral -> MxString

      is ThisLiteral -> env.classEnv?.type
        ?: throw ReferenceError(node.ctx, "Invalid use of this")

      is NewExpression -> {
        val ty = node.typeId.resolve(env)
        if (ty.isVoid) {
          throw TypeError(node.ctx, "New on incomplete type $ty")
        }
        checkNewArrayType(node.typeId, env)
        ty
      }
    }
  }

  private fun checkNewArrayType(node: NewTypeId, env: EnvironmentRecord) {
    if (node !is NewArrayType) return
    infer(node.length, env)
    val lengthType = node.length.currentType intersect MxInt
    if (lengthType != MxInt) {
      val msg = "Length has type ${node.length.currentType} (int expected)"
      throw TypeError(node.length.ctx, msg)
    }
    check(node.length, lengthType)
    if (node.typeId is NewTypeId) checkNewArrayType(node.typeId, env)
  }

  private fun check(node: Expression, type: Type) {
    val ty = type intersect node.currentType
    if (ty == node.currentType) return
    when (node) {
      is AssignmentExpression ->
        throw TypeError(node.ctx, "Assignment expression has no value")

      is NullLiteral -> {
        if (!(ty leq node.currentType)) {
          throw TypeError(node.ctx, "Illegal operand type")
        }
      }

      is BinaryExpression, is PrefixUpdateExpression, is PostfixUpdateExpression,
      is UnaryExpression, is Literal, is NewExpression ->
        throw TypeError(node.ctx, "Illegal operand type")

      is CallExpression -> {
        val calleeType = node.callee.currentType
        calleeType.checkCompleteness(node.ctx)
        if (calleeType !is MxFunction) {
          throw TypeError(node.ctx, "$calleeType is not a function")
        }
        if (node.arguments.size != calleeType.params.size) {
          throw MxcInternalError(node.ctx, "Call param count mismatch")
        }
        node.arguments.zip(calleeType.params)
          .forEach { check(it.first, it.second) }
        types[node] = ty intersect calleeType.returnType
        node.checkNotBot()
      }

      is GroupExpression -> {
        check(node.expression, type)
        types[node] = node.expression.currentType
      }

      is LambdaExpression -> {
        if (ty !is MxFunction) {
          throw TypeError(node.ctx, "$ty is not a function")
        }
        ty.returnType.checkNotBot(node.ctx)
        node.params.zip(ty.params).forEach {
          val name = it.first.id.name
          val current = node.env!!.getBinding(node.ctx, name).type
          val resolved = current intersect it.second
          resolved.checkNotBot(node.ctx)
          if (resolved != current) {
            val binding =
              Binding(it.first.ctx, name, resolved, Mutability.MUTABLE)
            node.env!!.setBinding(binding)
          }
        }
        check(node.body, ty.returnType)
      }

      is Identifier -> {
        val ref =
          references[node]!!.env.getIdentifierReference(node.ctx, node.name)
            ?: throw ReferenceError(node.ctx, "${node.name} is not defined")
        references[node] = ref
        types[node] = ty intersect ref.binding.type
        node.checkNotBot()
      }

      is ComputedMemberExpression -> {
        check(node.`object`, MxArray(ty))
        types[node] = ty
      }

      is MemberExpression -> {
        val objectType = node.`object`.currentType as ObjectType
        val binding = objectType.env.getBinding(node.ctx, node.prop.name)
        types[node] = ty intersect binding.type
        node.checkNotBot()
      }
    }
  }
}

private fun TypeId.resolve(env: EnvironmentRecord): Type = when (this) {
  is BoolType -> MxBool
  is IntType -> MxInt
  is VoidType -> MxVoid
  is StringType -> MxString

  is HoleType -> MxHole

  is ArrayType -> MxArray(typeId.resolve(env))
  is NewArrayType -> MxArray(typeId.resolve(env))
  is GroupedType -> typeId.resolve(env)

  is Identifier ->
    (env.getIdentifierReference(ctx, name)?.binding as? TypeBinding)?.content
      ?: throw TypeError(ctx, "'$name' does not name a type")

  is FunctionType ->
    MxFunction(params.map { it.resolve(env) }, returnType.resolve(env))
}

private fun FunctionDeclaration.type(env: EnvironmentRecord): Type =
  MxFunction(params.map { it.typeId.resolve(env) }, returnType.resolve(env))

private fun Type.checkCompleteness(ctx: SourceContext) {
  if (!complete) {
    throw TypeError(ctx, "Unable to deduce type (got $this)")
  }
}

private fun Type.checkNotBot(ctx: SourceContext) {
  if (this is MxBot) {
    throw TypeError(ctx, "Unable to deduce type")
  }
}
