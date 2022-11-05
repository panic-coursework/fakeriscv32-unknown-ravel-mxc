package org.altk.lab.mxc.ast

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
    is ComputedMemberExpression -> transform(node)
    is GroupExpression -> transform(node)
    is Identifier -> transform(node)
    is MemberExpression -> transform(node)
    is PrefixUpdateExpression -> transform(node)
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
        VariableDeclaration(decl.ctx, decl.typeId, decl.declarations.map { v ->
          VariableDeclarator(v.ctx, v.id, null)
        })
      } else if (decl is FunctionDeclaration && decl.id.name == "main") {
        FunctionDeclaration(
          decl.ctx, Identifier(decl.id.ctx, "mx.main"), decl.params,
          decl.returnType, decl.body,
        )
      } else {
        decl
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
