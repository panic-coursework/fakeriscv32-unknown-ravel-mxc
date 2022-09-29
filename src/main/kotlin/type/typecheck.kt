package org.altk.lab.mxc.type

import org.altk.lab.mxc.SyntaxError
import org.altk.lab.mxc.TypeError
import org.altk.lab.mxc.ast.Node as AstNode
import org.altk.lab.mxc.ast.*

fun typecheck(ast: Program) = TypecheckRecord(ast)

class TypecheckRecord(val ast: Program) {
  val globalEnv = createGlobalEnv()
  val envs = HashMap<AstNode, EnvironmentRecord>()
  val types = HashMap<Expression, Type>()
  val structs = HashMap<String, MxStruct>()
  val returnTypes = HashMap<Statement, Type>()
  val hasNormalCompletion = HashMap<Statement, Boolean>()

  private val AstNode.env get() = envs[this]
  private val Expression.currentType get() = types[this]
  private val Statement.currentReturnType get() = returnTypes[this]
  private val Statement.hasNormalCompletion
    get() = this@TypecheckRecord.hasNormalCompletion[this]

  init {
    envs[ast] = globalEnv

    collectTypes()

    collectClassFields()
    collectFunctionSignatures()

    collectGlobalVars()

    collectClassMethodsAndCtors()
    collectFunctions()
  }

  private val classes = ast.body.filterIsInstance<ClassDeclaration>()
  private val functions = ast.body.filterIsInstance<FunctionDeclaration>()

  private fun collectTypes() {
    for (class_ in classes) {
      val name = class_.id.name
      val env = EnvironmentRecord(globalEnv)
      envs[class_] = env
      val type = MxStruct(name, env)
      structs[name] = type
      val binding = TypeBinding(class_.ctx, name, type)
      globalEnv.createBinding(binding)
    }
  }

  private fun collectClassFields() {
    for (class_ in classes) {
      val env = envs[class_]!!

      for (decl in class_.body.filterIsInstance<VariableDeclaration>()) {
        val type = decl.typeId.type(env)
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

  private fun collectGlobalVars() {
    TODO()
  }

  private fun collectClassMethodsAndCtors() {
    TODO()
  }

  private fun collectFunctions() {
    TODO()
  }

  private fun check(node: Statement, env: EnvironmentRecord) {
    envs[node] = env
    hasNormalCompletion[node] = true
    when (node) {
      is BlockStatement -> {
        val blockEnv = EnvironmentRecord(env)
        envs[node] = blockEnv
        TODO()
      }

      is BreakStatement, is ContinueStatement -> {
        if (env.loopEnv == null) {
          throw SyntaxError(node.ctx, "loop statement not in loop")
        }
        hasNormalCompletion[node] = false
      }

      is EmptyStatement -> Unit
      is ExpressionStatement -> check(node.expression, env)
      is IfStatement -> {
        val ifEnv = EnvironmentRecord(env)
        envs[node] = ifEnv
        check(node.test, ifEnv)
        check(node.consequent, ifEnv)
        check(node.alternate, ifEnv)
        TODO()
      }

      is ForStatement -> TODO()
      is WhileStatement -> TODO()
      is ReturnStatement -> TODO()
      is VariableDeclaration -> TODO()
    }
  }

  private fun check(node: Expression, env: EnvironmentRecord) = when (node) {
    is AssignmentExpression -> {}
    is BinaryExpression -> TODO()
    is CallExpression -> TODO()
    is GroupExpression -> TODO()
    is LambdaExpression -> TODO()
    is ComputedMemberExpression -> TODO()
    is Identifier -> TODO()
    is MemberExpression -> TODO()
    is PrefixUpdateExpression -> TODO()
    is BooleanLiteral -> TODO()
    is IntegerLiteral -> TODO()
    is NullLiteral -> TODO()
    is StringLiteral -> TODO()
    is ThisLiteral -> TODO()
    is NewExpression -> TODO()
    is PostfixUpdateExpression -> TODO()
    is UnaryExpression -> TODO()
  }
}

private fun TypeId.type(env: EnvironmentRecord): Type = when (this) {
  is BoolType -> MxBool
  is IntType -> MxInt
  is VoidType -> MxVoid
  is StringType -> MxString

  is HoleType -> MxHole

  is ArrayType -> MxArray(typeId.type(env))
  is GroupedType -> typeId.type(env)

  is Identifier ->
    (env.getIdentifierReference(ctx, name)?.binding as? TypeBinding)?.type
      ?: throw TypeError(ctx, "'$name' does not name a type")

  is FunctionType ->
    MxFunction(params.map { it.type(env) }, returnType.type(env))

  is NewArrayType -> error("unreachable")
}

private fun FunctionDeclaration.type(env: EnvironmentRecord): Type =
  MxFunction(params.map { it.type.type(env) }, returnType.type(env))

