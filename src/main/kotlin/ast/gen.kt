package org.altk.lab.mxc.ast

import org.altk.lab.mxc.*
import org.altk.lab.mxc.recognizer.MxParser.*
import org.antlr.v4.runtime.ParserRuleContext

val ParserRuleContext.ctx: SourceContext
  get() = SourceContext(this)

private val IdentifierContext.ast: Identifier
  get() = ast(this)

private val TypeIdContext.ast: TypeId
  get() = ast(this)


fun ast(input: ProgramContext) =
  Program(input.ctx, input.programItem().map { ast(it) })


fun ast(input: ProgramItemContext): ProgramItem = when (input) {
  is ProgramFunctionContext -> ast(input)
  is ProgramClassContext -> ast(input)
  is ProgramLexicalContext -> ast(input)
  else -> throw MxcInternalError(input.ctx, "Unknown program item $input")
}

fun ast(input: ProgramFunctionContext) = ast(input.functionDeclaration())
fun ast(input: ProgramClassContext) = ast(input.classDeclaration())
fun ast(input: ProgramLexicalContext) = ast(input.lexicalDeclaration())


fun ast(input: FunctionDeclarationContext) = FunctionDeclaration(
  input.ctx,
  input.identifier().ast,
  input.parameterList().functionParameter().map { ast(it) },
  input.typeId().ast,
  ast(input.body),
)

fun ast(input: FunctionParameterContext) =
  FunctionParameter(input.ctx, input.identifier().ast, input.typeId().ast)


fun ast(input: ClassDeclarationContext) = ClassDeclaration(
  input.ctx,
  input.identifier().ast,
  input.classElement().map { ast(it) },
)

fun ast(input: ClassElementContext): ClassElement = when (input) {
  is ClassLexicalContext -> ast(input.lexicalDeclaration())
  is ClassConstructorContext -> ast(input.constructorDeclaration())
  is ClassFunctionContext -> ast(input.functionDeclaration())
  else -> throw MxcInternalError(input.ctx, "Unknown class element $input")
}

fun ast(input: ConstructorDeclarationContext) =
  ConstructorDeclaration(input.ctx, input.identifier().ast, ast(input.body))


fun ast(input: LexicalDeclarationContext) = VariableDeclaration(
  input.ctx,
  input.typeId().ast,
  input.lexicalBinding().map { ast(it) },
)

fun ast(input: LexicalBindingContext) = VariableDeclarator(
  input.ctx,
  input.identifier().ast,
  input.initializer?.let { ast(it) },
)


fun ast(input: StatementContext): Statement = when (input) {
  is StmtBlockContext -> ast(input.blockStatement())
  is StmtExpressionContext -> ast(input.expressionStatement())
  is StmtIfContext -> ast(input.ifStatement())
  is StmtIterationContext -> ast(input.iterationStatement())
  is StmtContinueContext -> ast(input.continueStatement())
  is StmtBreakContext -> ast(input.breakStatement())
  is StmtReturnContext -> ast(input.returnStatement())
  is StmtEmptyContext -> ast(input.emptyStatement())
  is StmtLexicalDeclarationContext -> ast(input.lexicalDeclaration())
  else -> throw MxcInternalError(input.ctx, "Unknown statement $input")
}

fun ast(input: BlockStatementContext) =
  BlockStatement(input.ctx, input.statement().map { ast(it) })

fun ast(input: ExpressionStatementContext) =
  ExpressionStatement(input.ctx, ast(input.expression()))

fun ast(input: IfStatementContext) = IfStatement(
  input.ctx,
  ast(input.test),
  ast(input.consequent),
  ast(input.alternate),
)

fun ast(input: IterationStatementContext): IterationStatement = when (input) {
  is LoopWhileContext -> ast(input)
  is LoopForDeclContext -> ast(input)
  is LoopForExprContext -> ast(input)
  else -> throw MxcInternalError(input.ctx, "Unknown iteration $input")
}

fun ast(input: LoopWhileContext) =
  WhileStatement(input.ctx, ast(input.test), ast(input.body))

fun ast(input: LoopForDeclContext) = ForStatement(
  input.ctx,
  ast(input.init),
  input.test?.let { ast(it) },
  input.update?.let { ast(it) },
  ast(input.body),
)

fun ast(input: LoopForExprContext) = ForStatement(
  input.ctx,
  input.init?.let { ast(it) },
  input.test?.let { ast(it) },
  input.update?.let { ast(it) },
  ast(input.body),
)

fun ast(input: ContinueStatementContext) = ContinueStatement(input.ctx)
fun ast(input: BreakStatementContext) = BreakStatement(input.ctx)
fun ast(input: EmptyStatementContext) = EmptyStatement(input.ctx)
fun ast(input: ReturnStatementContext) =
  ReturnStatement(input.ctx, input.argument?.let { ast(it) })


fun ast(input: ExpressionContext): Expression = when (input) {
  is ExprLhsContext -> ast(input.l)
  is ExprPostfixUpdateContext -> ast(input)
  is ExprUnaryContext -> ast(input)
  is ExprBinaryContext -> ast(input)
  is ExprAssignContext -> ast(input)
  else -> throw MxcInternalError(input.ctx, "Unknown expression $input")
}

fun ast(input: LeftHandSideExpressionContext): Expression = when (input) {
  is LhsIdentifierContext -> ast(input.identifier())
  is LhsLiteralContext -> ast(input.literalExpression())
  is LhsLambdaContext -> ast(input)
  is LhsNewContext -> ast(input)
  is LhsParenthesizedContext -> ast(input)
  is LhsMemberContext -> ast(input)
  is LhsComputedContext -> ast(input)
  is LhsPrefixUpdateContext -> ast(input)
  is LhsCallContext -> ast(input)
  else -> throw MxcInternalError(input.ctx, "Unknown expression $input")
}

private fun ensureLhs(input: Expression): LeftHandSideExpression {
  if (input !is LeftHandSideExpression) {
    throw SyntaxError(input.ctx, "Invalid left hand side expression")
  }
  return input
}

fun ast(input: ExprPostfixUpdateContext) = PostfixUpdateExpression(
  input.ctx,
  UpdateOperator.from(input.op.text),
  ensureLhs(ast(input.argument)),
)

fun ast(input: ExprUnaryContext) =
  UnaryExpression(input.ctx, UnaryOperator.from(input.op.text), ast(input.r))

fun ast(input: ExprBinaryContext) = BinaryExpression(
  input.ctx,
  BinaryOperator.from(input.op.text),
  ast(input.l),
  ast(input.r),
)

fun ast(input: ExprAssignContext) =
  AssignmentExpression(input.ctx, ensureLhs(ast(input.l)), ast(input.r))

fun ast(input: LhsLambdaContext) = LambdaExpression(
  input.ctx,
  input.capture != null,
  input.parameterList()?.functionParameter()?.map { ast(it) } ?: listOf(),
  ast(input.body),
)

fun ast(input: LhsNewContext) = NewExpression(input.ctx, ast(input.newTypeId()))

fun ast(input: LhsParenthesizedContext) =
  GroupExpression(input.ctx, ast(input.expression()))

fun ast(input: LhsMemberContext) = MemberExpression(
  input.ctx,
  ensureLhs(ast(input.`object`)),
  input.identifier().ast,
)

fun ast(input: LhsComputedContext) = ComputedMemberExpression(
  input.ctx,
  ensureLhs(ast(input.`object`)),
  ast(input.expression()),
)

fun ast(input: LhsPrefixUpdateContext) = PrefixUpdateExpression(
  input.ctx,
  UpdateOperator.from(input.op.text),
  ensureLhs(ast(input.argument)),
)

fun ast(input: LhsCallContext) = CallExpression(
  input.ctx,
  ast(input.callee),
  input.arguments().expression().map { ast(it) },
)


fun ast(input: LiteralExpressionContext): Literal = when (input) {
  is LitIntegerContext -> ast(input.integerLiteral())
  is LitStringContext -> ast(input.stringLiteral())
  is LitThisContext -> ast(input.thisLiteral())
  is LitBooleanContext -> ast(input.booleanLiteral())
  is LitNullContext -> ast(input.nullLiteral())
  else -> throw MxcInternalError(input.ctx, "Unknown literal $input")
}

fun ast(input: IntegerLiteralContext) =
  IntegerLiteral(input.ctx, parseInt(input))

fun ast(input: StringLiteralContext) =
  StringLiteral(input.ctx, unescape(input))

fun ast(input: BooleanLiteralContext) =
  BooleanLiteral(input.ctx, input.True() != null)

fun ast(input: ThisLiteralContext) = ThisLiteral(input.ctx)
fun ast(input: NullLiteralContext) = NullLiteral(input.ctx)

fun ast(input: IdentifierContext) = Identifier(input.ctx, input.text)


fun ast(input: TypeIdContext): TypeId = when (input) {
  is TypePrimitiveContext -> ast(input.primitiveTypeId())
  is TypeHoleContext -> HoleType(input.ctx)
  is TypeUserDefinedContext -> ast(input.identifier())
  is TypeArrayContext -> ArrayType(input.ctx, input.typeId().ast)
  is TypeParenthesizedContext -> GroupedType(input.ctx, input.typeId().ast)
  is TypeFunctionSingleParamContext -> ast(input)
  is TypeFunctionContext -> ast(input)
  else -> throw MxcInternalError(input.ctx, "Unknown type $input")
}

fun ast(input: PrimitiveTypeIdContext) = when (input) {
  is PrimitiveBoolContext -> BoolType(input.ctx)
  is PrimitiveIntContext -> IntType(input.ctx)
  is PrimitiveVoidContext -> VoidType(input.ctx)
  is PrimitiveStringContext -> StringType(input.ctx)
  else -> throw MxcInternalError(input.ctx, "Unknown primitive type $input")
}

fun ast(input: TypeFunctionSingleParamContext) = FunctionType(
  input.ctx,
  listOf(input.param.ast),
  input.returnType.ast,
)

fun ast(input: TypeFunctionContext) = FunctionType(
  input.ctx,
  input.params.typeId().map { it.ast },
  input.returnType.ast,
)


fun ast(input: NewTypeIdContext): NewTypeId = when (input) {
  is NewBasicContext -> ast(input.newTypeIdBasic())
  is NewArrayContext -> ast(input.newTypeIdArray())
  else -> throw MxcInternalError(input.ctx, "Unknown new type $input")
}

fun ast(input: NewTypeIdBasicContext) = input.identifier().ast
fun ast(input: NewTypeIdArrayContext): NewArrayType {
  val baseType = input.arrayBraces()
    .fold(input.base.ast) { type, _ -> ArrayType(input.ctx, type) }
  return input.expression().map { ast(it) }
    .foldRight(baseType) { length, base ->
      NewArrayType(input.ctx, base, length)
    } as NewArrayType
}
