package org.altk.lab.mxc.ast

import org.altk.lab.mxc.*
import kotlin.reflect.KProperty1
import kotlin.reflect.full.memberProperties

private fun valueToString(v: Any?): String = when (v) {
  null -> "null"
  is String -> "\"$v\"".replace("\\", "\\\\").replace("\\\\\"", "\\\"")
    .replace("\n", "\\n")

  is Boolean -> v.toString()
  is Node -> v.toString()
  is Int -> v.toString()
  is Double -> v.toString()
  is List<*> -> v.map { valueToString(it) }.toString()
  else -> "\"$v\""
}

sealed class Node(val ctx: SourceContext) {
  @Suppress("UNCHECKED_CAST")
  override fun toString(): String {
    val className = this::class.simpleName
    val fields = this::class.memberProperties.filter {
      it.name != "ctx"
    }.joinToString {
      it as KProperty1<Node, *>
      val value = valueToString(it.get(this))
      "\"${it.name}\": $value"
    }
    val comma = if (fields.isNotEmpty()) ", " else ""
    return "{\"type\": \"$className\"$comma$fields}"
  }
}

sealed interface ProgramItem
class Program(
  ctx: SourceContext,
  val body: List<ProgramItem>,
) : Node(ctx)

class FunctionDeclaration(
  ctx: SourceContext,
  val id: Identifier,
  val params: List<FunctionParameter>,
  val returnType: TypeId,
  val body: BlockStatement,
) : Node(ctx), ProgramItem, ClassElement

class FunctionParameter(
  ctx: SourceContext,
  val id: Identifier,
  val type: TypeId,
) : Node(ctx)

class VariableDeclaration(
  ctx: SourceContext,
  val typeId: TypeId,
  val declarations: List<VariableDeclarator>,
) : Statement(ctx), ProgramItem, ClassElement, ForInit

class VariableDeclarator(
  ctx: SourceContext,
  val id: Identifier,
  val init: Expression?,
) : Node(ctx)

sealed interface ClassElement
class ClassDeclaration(
  ctx: SourceContext,
  val id: Identifier,
  val body: List<ClassElement>,
) : Node(ctx), ProgramItem

class ConstructorDeclaration(
  ctx: SourceContext,
  val id: Identifier,
  val body: BlockStatement,
) : Node(ctx), ClassElement

sealed class Statement(ctx: SourceContext) : Node(ctx)

class BlockStatement(
  ctx: SourceContext,
  val body: List<Statement>,
) : Statement(ctx)

class ExpressionStatement(
  ctx: SourceContext,
  val expression: Expression,
) : Statement(ctx)

class IfStatement(
  ctx: SourceContext,
  val test: Expression,
  val consequent: Statement,
  val alternate: Statement,
) : Statement(ctx)

sealed class IterationStatement(ctx: SourceContext) : Statement(ctx)

class WhileStatement(
  ctx: SourceContext,
  val test: Expression,
  val body: Statement,
) : IterationStatement(ctx)

sealed interface ForInit
class ForStatement(
  ctx: SourceContext,
  val init: ForInit?,
  val test: Expression?,
  val update: Expression?,
  val body: Statement,
) : IterationStatement(ctx)

class ContinueStatement(ctx: SourceContext) : Statement(ctx)
class BreakStatement(ctx: SourceContext) : Statement(ctx)
class EmptyStatement(ctx: SourceContext) : Statement(ctx)
class ReturnStatement(ctx: SourceContext, val argument: Expression?) :
  Statement(ctx)

sealed class Expression(ctx: SourceContext) : Node(ctx), ForInit
sealed class LeftHandSideExpression(ctx: SourceContext) : Expression(ctx)

class Identifier(ctx: SourceContext, val name: String) :
  LeftHandSideExpression(ctx), TypeId, NewTypeId

sealed class Literal(ctx: SourceContext) : Expression(ctx)
class IntegerLiteral(ctx: SourceContext, val value: Int) : Literal(ctx)
class StringLiteral(ctx: SourceContext, val value: String) : Literal(ctx)
class ThisLiteral(ctx: SourceContext) : Literal(ctx)
class BooleanLiteral(ctx: SourceContext, val value: Boolean) : Literal(ctx)
class NullLiteral(ctx: SourceContext) : Literal(ctx)

class LambdaExpression(
  ctx: SourceContext,
  val capture: Boolean,
  val params: List<FunctionParameter>,
  val body: BlockStatement,
) : Expression(ctx)

class NewExpression(ctx: SourceContext, val typeId: NewTypeId) :
  Expression(ctx)

class GroupExpression(ctx: SourceContext, val expression: Expression) :
  Expression(ctx)

class MemberExpression(
  ctx: SourceContext,
  val `object`: Expression,
  val prop: Identifier,
) : LeftHandSideExpression(ctx)

class ComputedMemberExpression(
  ctx: SourceContext,
  val `object`: Expression,
  val prop: Expression,
) : LeftHandSideExpression(ctx)

class PrefixUpdateExpression(
  ctx: SourceContext,
  val operator: UpdateOperator,
  val argument: LeftHandSideExpression,
) : LeftHandSideExpression(ctx)

class CallExpression(
  ctx: SourceContext,
  val callee: Expression,
  val arguments: List<Expression>,
) : Expression(ctx)

class PostfixUpdateExpression(
  ctx: SourceContext,
  val operator: UpdateOperator,
  val argument: LeftHandSideExpression,
) : Expression(ctx)

class UnaryExpression(
  ctx: SourceContext,
  val operator: UnaryOperator,
  val argument: Expression,
) : Expression(ctx)

class BinaryExpression(
  ctx: SourceContext,
  val operator: BinaryOperator,
  val left: Expression,
  val right: Expression,
) : Expression(ctx)

class AssignmentExpression(
  ctx: SourceContext,
  val left: LeftHandSideExpression,
  val right: Expression,
) : Expression(ctx)

sealed interface TypeId
sealed interface NewTypeId
sealed class PrimitiveType(ctx: SourceContext) : Node(ctx), TypeId

class BoolType(ctx: SourceContext) : PrimitiveType(ctx)
class IntType(ctx: SourceContext) : PrimitiveType(ctx)
class VoidType(ctx: SourceContext) : PrimitiveType(ctx)
class StringType(ctx: SourceContext) : PrimitiveType(ctx)

class HoleType(ctx: SourceContext) : Node(ctx), TypeId

class ArrayType(ctx: SourceContext, val typeId: TypeId) : Node(ctx), TypeId

class GroupedType(ctx: SourceContext, val typeId: TypeId) : Node(ctx), TypeId
class FunctionType(
  ctx: SourceContext,
  val params: List<TypeId>,
  val returnType: TypeId,
) : Node(ctx), TypeId

class NewArrayType(
  ctx: SourceContext,
  val typeId: TypeId,
  val length: Expression,
) : Node(ctx), TypeId, NewTypeId

enum class UnaryOperator {
  NOT, BIT_NOT, POS, NEG;

  companion object {
    fun from(input: String): UnaryOperator = when (input) {
      "+" -> POS
      "-" -> NEG
      "!" -> NOT
      "~" -> BIT_NOT
      else -> throw MxcInternalError(null, "Unknown unary operator $input")
    }
  }
}

enum class UpdateOperator {
  INC, DEC;

  companion object {
    fun from(input: String): UpdateOperator = when (input) {
      "++" -> INC
      "--" -> DEC
      else -> throw MxcInternalError(null, "Unknown update operator $input")
    }
  }
}

enum class BinaryOperator {
  ADD, SUB, MUL, DIV, REM, SHL, SHR,
  LE, GE, LT, GT, EQ, NE,
  BIT_AND, BIT_OR, BIT_XOR,
  AND, OR,
  ;

  companion object {
    fun from(input: String): BinaryOperator = when (input) {
      "+" -> ADD
      "-" -> SUB
      "*" -> MUL
      "/" -> DIV
      "%" -> REM
      "<<" -> SHL
      ">>" -> SHR
      "<=" -> LE
      ">=" -> GE
      "<" -> LT
      ">" -> GT
      "==" -> EQ
      "!=" -> NE
      "&" -> BIT_AND
      "|" -> BIT_OR
      "^" -> BIT_XOR
      "&&" -> AND
      "||" -> OR
      else -> throw MxcInternalError(null, "Unknown binary operator $input")
    }
  }
}
