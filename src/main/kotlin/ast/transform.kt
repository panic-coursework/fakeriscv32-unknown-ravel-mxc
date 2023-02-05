package org.altk.lab.mxc.ast

import org.altk.lab.mxc.type.ClassEnvironmentRecord
import org.altk.lab.mxc.type.Mutability
import org.altk.lab.mxc.type.TypecheckRecord

abstract class Transformer {
  open fun transform(node: Program) =
    Program(node.ctx, node.body.map { transform(it) })

  open fun transform(node: ProgramItem): ProgramItem = when (node) {
    is ClassDeclaration -> transform(node)
    is FunctionDeclaration -> transform(node)
    is VariableDeclaration -> transform(node)
  }

  open fun transform(node: ClassDeclaration) = ClassDeclaration(
    node.ctx,
    transform(node.id),
    node.body.map { transform(it) },
  )

  open fun transform(node: ClassElement): ClassElement = when (node) {
    is ConstructorDeclaration -> transform(node)
    is FunctionDeclaration -> transform(node)
    is VariableDeclaration -> transform(node)
  }

  open fun transform(node: ConstructorDeclaration) =
    ConstructorDeclaration(node.ctx, transform(node.id), transform(node.body))

  open fun transform(node: FunctionDeclaration) = FunctionDeclaration(
    node.ctx,
    transform(node.id),
    node.params.map { transform(it) },
    transform(node.returnType),
    transform(node.body),
  )

  open fun transform(node: FunctionParameter) =
    FunctionParameter(node.ctx, transform(node.id), transform(node.typeId))

  open fun transform(node: VariableDeclaration) = VariableDeclaration(
    node.ctx,
    transform(node.typeId),
    node.declarations.map { transform(it) },
  )

  open fun transform(node: VariableDeclarator) = VariableDeclarator(
    node.ctx,
    transform(node.id),
    node.init?.let { transform(it) },
  )

  open fun transform(node: Identifier) = node
  open fun transform(node: BlockStatement) =
    BlockStatement(node.ctx, node.body.map { transform(it) })

  open fun transform(node: Statement): Statement = when (node) {
    is BlockStatement -> transform(node)
    is BreakStatement -> transform(node)
    is ContinueStatement -> transform(node)
    is EmptyStatement -> transform(node)
    is ExpressionStatement -> transform(node)
    is IfStatement -> transform(node)
    is ForStatement -> transform(node)
    is WhileStatement -> transform(node)
    is ReturnStatement -> transform(node)
    is VariableDeclaration -> transform(node)
  }

  open fun transform(node: BreakStatement) = node
  open fun transform(node: ContinueStatement) = node
  open fun transform(node: EmptyStatement) = node
  open fun transform(node: ExpressionStatement) =
    ExpressionStatement(node.ctx, transform(node.expression))

  open fun transform(node: IfStatement) = IfStatement(
    node.ctx,
    transform(node.test),
    transform(node.consequent),
    node.alternate?.let { transform(it) },
  )

  open fun transform(node: ForStatement) = ForStatement(
    node.ctx,
    node.init?.let { transform(it) },
    node.test?.let { transform(it) },
    node.update?.let { transform(it) },
    transform(node.body),
  )

  open fun transform(node: ForInit): ForInit = when (node) {
    is Expression -> transform(node)
    is VariableDeclaration -> transform(node)
  }

  open fun transform(node: WhileStatement) = WhileStatement(
    node.ctx,
    transform(node.test),
    transform(node.body),
  )

  open fun transform(node: ReturnStatement) = ReturnStatement(
    node.ctx,
    node.argument?.let { transform(it) },
  )

  open fun transform(node: Expression): Expression = when (node) {
    is AssignmentExpression -> transform(node)
    is BinaryExpression -> transform(node)
    is CallExpression -> transform(node)
    is LambdaExpression -> transform(node)
    is LeftHandSideExpression -> transform(node)
    is Literal -> transform(node)
    is NewExpression -> transform(node)
    is PostfixUpdateExpression -> transform(node)
    is UnaryExpression -> transform(node)
  }

  open fun transform(node: Literal): Literal = when (node) {
    is BooleanLiteral -> transform(node)
    is IntegerLiteral -> transform(node)
    is NullLiteral -> transform(node)
    is StringLiteral -> transform(node)
    is ThisLiteral -> transform(node)
  }

  open fun transform(node: AssignmentExpression) = AssignmentExpression(
    node.ctx,
    transform(node.left),
    transform(node.right),
  )

  open fun transform(node: BinaryExpression) = BinaryExpression(
    node.ctx,
    transform(node.operator),
    transform(node.left),
    transform(node.right),
  )

  open fun transform(node: BinaryOperator) = node
  open fun transform(node: LeftHandSideExpression): LeftHandSideExpression =
    when (node) {
      is ComputedMemberExpression -> transform(node)
      is GroupExpression -> transform(node)
      is Identifier -> transform(node)
      is MemberExpression -> transform(node)
      is PrefixUpdateExpression -> transform(node)
    }

  open fun transform(node: CallExpression) = CallExpression(
    node.ctx,
    transform(node.callee),
    node.arguments.map { transform(it) },
  )

  open fun transform(node: LambdaExpression) = LambdaExpression(
    node.ctx,
    node.capture,
    node.params.map { transform(it) },
    transform(node.body),
  )

  open fun transform(node: ComputedMemberExpression) = ComputedMemberExpression(
    node.ctx,
    transform(node.`object`),
    transform(node.prop),
  )

  open fun transform(node: BooleanLiteral) = node
  open fun transform(node: IntegerLiteral) = node
  open fun transform(node: NullLiteral) = node
  open fun transform(node: StringLiteral) = node
  open fun transform(node: ThisLiteral) = node

  open fun transform(node: GroupExpression) =
    GroupExpression(node.ctx, transform(node.expression))

  open fun transform(node: NewExpression) =
    NewExpression(node.ctx, transform(node.typeId))

  open fun transform(node: UpdateOperator) = node
  open fun transform(node: PostfixUpdateExpression) = PostfixUpdateExpression(
    node.ctx,
    transform(node.operator),
    transform(node.argument),
  )

  open fun transform(node: PrefixUpdateExpression) = PrefixUpdateExpression(
    node.ctx,
    transform(node.operator),
    transform(node.argument),
  )

  open fun transform(node: UnaryOperator) = node
  open fun transform(node: UnaryExpression) = UnaryExpression(
    node.ctx,
    transform(node.operator),
    transform(node.argument),
  )

  open fun transform(node: MemberExpression) = MemberExpression(
    node.ctx,
    transform(node.`object`),
    transform(node.prop),
  )

  open fun transform(node: TypeId): TypeId = when (node) {
    is ArrayType -> transform(node)
    is FunctionType -> transform(node)
    is GroupedType -> transform(node)
    is HoleType -> transform(node)
    is Identifier -> transform(node)
    is NewArrayType -> transform(node)
    is BoolType -> transform(node)
    is IntType -> transform(node)
    is StringType -> transform(node)
    is VoidType -> transform(node)
  }

  open fun transform(node: NewTypeId): NewTypeId = when (node) {
    is HoleType -> transform(node)
    is Identifier -> transform(node)
    is NewArrayType -> transform(node)
  }

  open fun transform(node: BoolType) = node
  open fun transform(node: IntType) = node
  open fun transform(node: StringType) = node
  open fun transform(node: VoidType) = node
  open fun transform(node: HoleType) = node
  open fun transform(node: ArrayType) =
    ArrayType(node.ctx, transform(node.typeId))

  open fun transform(node: GroupedType) =
    GroupedType(node.ctx, transform(node.typeId))

  open fun transform(node: FunctionType) = FunctionType(
    node.ctx,
    node.params.map { transform(it) },
    transform(node.returnType),
  )

  open fun transform(node: NewArrayType) = NewArrayType(
    node.ctx,
    transform(node.typeId),
    transform(node.length),
  )
}

class InjectReturnZeroToMain : Transformer() {
  override fun transform(node: Program) =
    Program(node.ctx, node.body.map { decl ->
      if (decl is FunctionDeclaration && decl.id.name == "main") {
        val ret = ReturnStatement(
          BuiltinSourceContext,
          IntegerLiteral(BuiltinSourceContext, 0),
        )
        FunctionDeclaration(
          decl.ctx, decl.id, decl.params, decl.returnType,
          BlockStatement(decl.body.ctx, decl.body.body + ret),
        )
      } else {
        decl
      }
    })
}

class MoveGlobalVarsToMain : Transformer() {
  override fun transform(node: Program) =
    Program(node.ctx, node.body.map { decl ->
      if (decl is VariableDeclaration) {
        VariableDeclaration(
          decl.ctx,
          transform(decl.typeId),
          decl.declarations.map { v ->
            transform(VariableDeclarator(v.ctx, v.id, null))
          })
      } else {
        transform(decl)
      }
    } + getGlobalVarsMain(node))

  override fun transform(node: Identifier) = if (node.name == "main") {
    Identifier(node.ctx, "mx.main")
  } else {
    node
  }

  private fun getGlobalVarsMain(program: Program): FunctionDeclaration {
    val initStatements = program.body
      .filterIsInstance<VariableDeclaration>()
      .flatMap { decl ->
        decl.declarations.filter { it.init != null }.map { v ->
          ExpressionStatement(
            v.init!!.ctx,
            AssignmentExpression(v.init.ctx, v.id, v.init),
          )
        }
      }
    val callMain = ReturnStatement(
      BuiltinSourceContext,
      CallExpression(
        BuiltinSourceContext,
        Identifier(BuiltinSourceContext, "mx.main"),
        listOf(),
      ),
    )
    return FunctionDeclaration(
      BuiltinSourceContext,
      Identifier(BuiltinSourceContext, "main"),
      listOf(),
      IntType(BuiltinSourceContext),
      BlockStatement(BuiltinSourceContext, initStatements + callMain),
    )
  }
}

class GenerateEmptyConstructors : Transformer() {
  override fun transform(node: ClassDeclaration) =
    if (node.constructor != null) {
      node
    } else {
      val ctor = ConstructorDeclaration(
        BuiltinSourceContext,
        node.id,
        BlockStatement(BuiltinSourceContext, listOf()),
      )
      ClassDeclaration(node.ctx, node.id, node.body + ctor)
    }
}

class DesugarMultiDimensionalNewExpressions : Transformer() {
  private val extraFunctions = mutableListOf<FunctionDeclaration>()

  override fun transform(node: Program) = Program(
    node.ctx,
    node.body.map { transform(it) } + extraFunctions,
  )

  override fun transform(node: Expression) =
    if (node is NewExpression && node.typeId is NewArrayType && node.typeId.typeId is NewArrayType) {
      fun baseType(ty: TypeId): TypeId =
        (ty as? NewArrayType)?.let {
          ArrayType(BuiltinSourceContext, baseType(it.typeId))
        } ?: ty

      fun forStatement(ty: NewArrayType, depth: Int): ForStatement =
        ForStatement(
          BuiltinSourceContext,
          VariableDeclaration(
            BuiltinSourceContext,
            IntType(BuiltinSourceContext),
            listOf(
              VariableDeclarator(
                BuiltinSourceContext,
                Identifier(BuiltinSourceContext, "i$depth"),
                IntegerLiteral(BuiltinSourceContext, 0),
              ),
            ),
          ),
          BinaryExpression(
            BuiltinSourceContext,
            BinaryOperator.LT,
            Identifier(BuiltinSourceContext, "i$depth"),
            Identifier(BuiltinSourceContext, "n$depth"),
          ),
          PrefixUpdateExpression(
            BuiltinSourceContext,
            UpdateOperator.INC,
            Identifier(BuiltinSourceContext, "i$depth"),
          ),
          BlockStatement(
            BuiltinSourceContext,
            listOf(
              VariableDeclaration(
                BuiltinSourceContext,
                HoleType(BuiltinSourceContext),
                listOf(
                  VariableDeclarator(
                    BuiltinSourceContext,
                    Identifier(BuiltinSourceContext, "a${depth + 1}"),
                    NewExpression(
                      BuiltinSourceContext,
                      NewArrayType(
                        BuiltinSourceContext,
                        baseType(ty.typeId),
                        Identifier(BuiltinSourceContext, "n${depth + 1}"),
                      ),
                    ),
                  ),
                ),
              ),
              ExpressionStatement(
                BuiltinSourceContext,
                AssignmentExpression(
                  BuiltinSourceContext,
                  ComputedMemberExpression(
                    BuiltinSourceContext,
                    Identifier(BuiltinSourceContext, "a$depth"),
                    Identifier(BuiltinSourceContext, "i$depth"),
                  ),
                  Identifier(BuiltinSourceContext, "a${depth + 1}"),
                ),
              ),
            ) + if (ty.typeId is NewArrayType) {
              listOf(forStatement(ty.typeId, depth + 1))
            } else {
              listOf()
            },
          ),
        )

      val topLevelNew = VariableDeclaration(
        BuiltinSourceContext,
        HoleType(BuiltinSourceContext),
        listOf(
          VariableDeclarator(
            BuiltinSourceContext,
            Identifier(BuiltinSourceContext, "a0"),
            NewExpression(
              BuiltinSourceContext,
              NewArrayType(
                BuiltinSourceContext,
                baseType(node.typeId.typeId),
                Identifier(BuiltinSourceContext, "n0"),
              ),
            ),
          ),
        ),
      )
      val ret = ReturnStatement(
        BuiltinSourceContext,
        Identifier(BuiltinSourceContext, "a0"),
      )

      fun arguments(ty: NewArrayType): List<Expression> =
        listOf(ty.length) + ((ty.typeId as? NewArrayType)?.let { arguments(ty.typeId) }
          ?: listOf())

      val name = "new.${extraFunctions.size}"
      val args = arguments(node.typeId)
      val body = listOf(topLevelNew) + forStatement(node.typeId.typeId, 0) + ret
      val func = FunctionDeclaration(
        BuiltinSourceContext,
        Identifier(BuiltinSourceContext, name),
        List(args.size) { i ->
          FunctionParameter(
            BuiltinSourceContext,
            Identifier(BuiltinSourceContext, "n$i"),
            IntType(BuiltinSourceContext),
          )
        },
        baseType(node.typeId),
        BlockStatement(BuiltinSourceContext, body),
      )
      extraFunctions.add(func)
      CallExpression(
        BuiltinSourceContext,
        Identifier(BuiltinSourceContext, name),
        args,
      )
    } else {
      super.transform(node)
    }
}

class DesugarConstructors : Transformer() {
  override fun transform(node: ClassElement) =
    if (node is ConstructorDeclaration) {
      FunctionDeclaration(
        node.ctx,
        Identifier(node.id.ctx, "(init)"),
        listOf(),
        VoidType(BuiltinSourceContext),
        node.body,
      )
    } else {
      node
    }
}

class DesugarClassFields : Transformer() {
  private lateinit var tyck: TypecheckRecord
  override fun transform(node: Program): Program {
    tyck = TypecheckRecord(node)
    return super.transform(node)
  }

  override fun transform(node: LeftHandSideExpression): LeftHandSideExpression =
    if (node is Identifier && tyck.references[node]!!.env is ClassEnvironmentRecord) {
      MemberExpression(
        BuiltinSourceContext,
        ThisLiteral(BuiltinSourceContext),
        node,
      )
    } else {
      super.transform(node)
    }
}

class OldLocalizeGlobalVariables : Transformer() {
  private lateinit var tyck: TypecheckRecord
  private val usages = HashMap<String, HashSet<FunctionDeclaration>>()
  private val reverseUsages =
    HashMap<FunctionDeclaration, MutableList<String>>()
  private val mutated = HashSet<String>()
  private val hasCalls = HashSet<FunctionDeclaration>()

  private inner class CollectUsages : Transformer() {
    private var currentFunction: FunctionDeclaration? = null
    private var loop = 0
    override fun transform(node: FunctionDeclaration): FunctionDeclaration {
      currentFunction = node
      reverseUsages[node] = mutableListOf()
      try {
        return super.transform(node)
      } finally {
        currentFunction = null
      }
    }

    override fun transform(node: ForStatement): ForStatement {
      try {
        ++loop
        return super.transform(node)
      } finally {
        --loop
      }
    }

    override fun transform(node: Identifier): Identifier {
      val func = currentFunction
      val ref = tyck.references[node]
      if (func != null && ref?.isGlobal == true) {
        if (loop > 0) {
          (0..9).forEach { _ -> usages[node.name]?.add(func) }
        }
        usages[node.name]?.add(func)
        if (ref.binding.mutability == Mutability.MUTABLE) {
          reverseUsages[func]!!.add(node.name)
        }
      }
      return node
    }

    override fun transform(node: AssignmentExpression): AssignmentExpression {
      if (node.left is Identifier && tyck.references[node.left]!!.isGlobal) {
        mutated.add(node.left.name)
      }
      return super.transform(node)
    }

    override fun transform(node: CallExpression): CallExpression {
      if (currentFunction != null) hasCalls.add(currentFunction!!)
      return super.transform(node)
    }
  }

  private val preamble =
    HashMap<FunctionDeclaration, MutableList<VariableDeclaration>>()
  private val localizedVars =
    HashMap<FunctionDeclaration, MutableList<VariableDeclaration>>()

  override fun transform(node: Program): Program {
    tyck = TypecheckRecord(node)
    val reverse = node.body
      .filterIsInstance<VariableDeclaration>()
      .flatMap { d -> d.declarations.map { Triple(it.id.name, d, it) } }
      .associateBy { it.first }
      .mapValues { (_, v) -> Pair(v.second, v.third) }
    for (name in reverse.keys) {
      usages[name] = HashSet()
    }
    CollectUsages().transform(node)

    val removedDeclarators = HashSet<VariableDeclarator>()
    val removedGlobals = HashSet<String>()
    for ((name, fs) in usages) {
      if (fs.size != 1) continue
      val func = fs.first()
      if (func.params.isNotEmpty()) continue
      val (declaration, declarator) = reverse[name]!!
      if (declarator.init != null) {
        if (name in mutated) continue
        val init = declarator.init
        if (
          init !is NewExpression ||
          init.typeId !is NewArrayType ||
          init.typeId.typeId is NewArrayType ||
          init.typeId.length !is IntegerLiteral
        ) continue
      }
//      System.err.println("moving $name into ${func.id.name}")
      val stmts = preamble.getOrPut(func) { mutableListOf() }
      stmts.add(
        VariableDeclaration(
          declaration.ctx,
          declaration.typeId,
          listOf(declarator),
        )
      )
      removedDeclarators.add(declarator)
      removedGlobals.add(name)
    }

    for ((func, usedGlobals) in reverseUsages) {
      if (!removedGlobals.containsAll(usedGlobals)) {
        if (func !in hasCalls) for (v in usedGlobals.toSet()) {
          val count = usedGlobals.count { it == v }
//          System.err.println("$v $count")
          // replaced by IR transform
          if (count > 1048576) {
            val (declaration, _) = reverse[v]!!
            val newId = "global.$v"
            localizedVars.getOrPut(func) { mutableListOf() }.add(
              VariableDeclaration(
                declaration.ctx,
                declaration.typeId,
                listOf(
                  VariableDeclarator(
                    BuiltinSourceContext,
                    Identifier(BuiltinSourceContext, newId),
                    Identifier(BuiltinSourceContext, v),
                  ),
                ),
              ),
            )
          }
        }

        val decls = preamble[func]?.map { it.declarations.first() } ?: continue
        removedDeclarators.removeAll(decls.toSet())
        preamble.remove(func)
      }
    }

    val prog = Program(node.ctx, node.body.mapNotNull { item ->
      if (item is VariableDeclaration) {
        val decls = item.declarations.filter { it !in removedDeclarators }
        if (decls.isEmpty()) {
          null
        } else {
          VariableDeclaration(item.ctx, item.typeId, decls)
        }
      } else {
        item
      }
    })

    return super.transform(prog)
  }

  private val localizedIds = HashSet<String>()
  private lateinit var saves: List<ExpressionStatement>
  override fun transform(node: FunctionDeclaration): FunctionDeclaration {
    val globals = preamble[node]
    if (globals == null) {
      val loads = localizedVars[node] ?: return node
      saves = loads.map { v ->
        val name = (v.declarations.first().init as Identifier).name
        localizedIds.add(name)
        ExpressionStatement(
          BuiltinSourceContext,
          AssignmentExpression(
            BuiltinSourceContext,
            Identifier(BuiltinSourceContext, name),
            Identifier(BuiltinSourceContext, "global.$name"),
          ),
        )
      }
      try {
        return FunctionDeclaration(
          node.ctx, node.id, node.params, node.returnType,
          BlockStatement(node.body.ctx, loads + transform(node.body) + saves),
        )
      } finally {
        localizedIds.clear()
      }
    }
    return FunctionDeclaration(
      node.ctx, node.id, node.params, node.returnType,
      BlockStatement(node.body.ctx, globals + node.body),
    )
  }

  override fun transform(node: Identifier): Identifier {
    val ref = tyck.references[node] ?: return node
    if (!ref.isGlobal || node.name !in localizedIds) return node
    return Identifier(node.ctx, "global.${node.name}")
  }

  override fun transform(node: Statement) = if (node is ReturnStatement) {
    BlockStatement(node.ctx, saves + transform(node))
  } else {
    super.transform(node)
  }
}

class MemoizePureFunctions : Transformer() {
  private lateinit var tyck: TypecheckRecord
  override fun transform(node: Program): Program {
    tyck = TypecheckRecord(node)
    super.transform(node)
    val extras = memoizeFunctions.associateWith { func ->
      fun statusId() = Identifier(func.id.ctx, "${func.id.name}.uninitialized")
      fun memoizedId() = Identifier(func.id.ctx, "${func.id.name}.memoized")
      fun renamedId() = Identifier(func.id.ctx, "${func.id.name}.call")
      val ctx = BuiltinSourceContext
      val renamed = FunctionDeclaration(
        func.ctx,
        renamedId(),
        func.params,
        func.returnType,
        func.body,
      )
      val status = VariableDeclaration(
        ctx, BoolType(ctx),
        listOf(VariableDeclarator(ctx, statusId(), BooleanLiteral(ctx, true))),
      )
      val decl = VariableDeclaration(
        ctx, func.returnType,
        listOf(VariableDeclarator(ctx, memoizedId(), null)),
      )
      val stub = FunctionDeclaration(
        ctx, func.id,
        listOf(),
        func.returnType,
        BlockStatement(
          ctx,
          listOf(
            IfStatement(
              ctx, statusId(),
              BlockStatement(
                ctx,
                listOf(
                  ExpressionStatement(
                    ctx,
                    AssignmentExpression(
                      ctx, statusId(),
                      BooleanLiteral(ctx, false),
                    ),
                  ),
                  ExpressionStatement(
                    ctx,
                    AssignmentExpression(
                      ctx, memoizedId(),
                      CallExpression(ctx, renamedId(), listOf()),
                    ),
                  ),
                ),
              ),
              null,
            ),
            ReturnStatement(ctx, memoizedId()),
          ),
        ),
      )
      listOf(renamed, status, decl, stub)
    }

    val body = node.body.flatMap {
      if (it in memoizeFunctions) extras[it]!! else listOf(it)
    }

    return Program(node.ctx, body)
  }

  private var pure = true
  private val memoizeFunctions = HashSet<FunctionDeclaration>()
  override fun transform(node: FunctionDeclaration): FunctionDeclaration {
    if (node.params.isNotEmpty() || node.id.name == "main" || node.returnType is VoidType) {
      return node
    }
    pure = true
    super.transform(node)
    if (pure) {
      memoizeFunctions.add(node)
    }
    return node
  }

  override fun transform(node: CallExpression): CallExpression {
    pure = false
    return node
  }

  override fun transform(node: Identifier): Identifier {
    if (tyck.references[node]?.isGlobal == true) {
      pure = false
    }
    return node
  }
}
