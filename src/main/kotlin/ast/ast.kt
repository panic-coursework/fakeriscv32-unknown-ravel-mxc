package org.altk.lab.mxc.ast

import org.altk.lab.mxc.*
import org.altk.lab.mxc.typechecker.*
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
  override fun toString(): String {
    val className = this::class.simpleName
    val fields = this::class.memberProperties.filter {
      it.name != "ctx"
    }.joinToString {
      it as KProperty1<Node, *>
      val value = valueToString(it.get(this))
      "\"${it.name}\":$value"
    }
    return "{\"type\":\"$className\",$fields}"
  }
}

sealed interface ProgramItem
class Program(
  ctx: SourceContext,
  val body: List<ProgramItem>,
  override val env: EnvironmentRecord,
) : Node(ctx), Scope

class FunctionDeclaration(
  ctx: SourceContext,
  val id: Identifier,
  val params: List<FunctionParam>,
  val returnType: Type,
  val body: BlockStatement,
) : Node(ctx), ProgramItem, ClassElement, Term {
  override val type: Type
    get() {
      val ret = returnType.intersect(ctx, body.type)
      if (!ret.complete) throw TypeError(ctx, "Cannot deduce return type")
      return MxFunction(params.map { it.typeId.type }, ret)
    }
}

class FunctionParam(
  ctx: SourceContext,
  val id: Identifier,
  val typeId: TypeId,
) : Node(ctx)

class VariableDeclaration(
  ctx: SourceContext,
  val declarations: List<VariableDeclarator>,
) : Node(ctx), ProgramItem, ClassElement

class VariableDeclarator(
  ctx: SourceContext,
  val id: Identifier,
  val typeId: TypeId,
  val init: Expression?,
) : Node(ctx)

sealed interface ClassElement
class ClassDeclaration(
  ctx: SourceContext,
  val id: Identifier,
  val body: List<ClassElement>,
  override val env: EnvironmentRecord,
) : Node(ctx), Scope, ProgramItem

class ConstructorDeclaration(
  ctx: SourceContext,
  val id: Identifier,
  val body: BlockStatement,
) : Node(ctx), ClassElement

sealed interface Statement

class BlockStatement(
  ctx: SourceContext,
  val body: List<Statement>,
  override val env: EnvironmentRecord,
) : Node(ctx), Statement, Scope, Term {
  override val type: Type = TODO("Not yet implemented")
}

class ExpressionStatement(
  ctx: SourceContext,
  val expression: Expression,
) : Node(ctx), Statement, Term {
  override val type = expression.type
}

sealed interface Expression : Term

class Identifier(ctx: SourceContext, val name: String) : Node(ctx), Expression {
  override val type = ctx.env.getIdentifierReference(ctx, name)?.binding?.type
    ?: throw ReferenceError(ctx, "Identifier $name not declared")
}

sealed class Literal(ctx: SourceContext) : Node(ctx), Expression
class IntegerLiteral(ctx: SourceContext, val value: Int) : Literal(ctx) {
  override val type = MxInt
}

class StringLiteral(ctx: SourceContext, val value: String) : Literal(ctx) {
  override val type = MxString
}

class ThisLiteral(ctx: SourceContext) : Literal(ctx) {
  override val type = ctx.env.thisType
    ?: throw ReferenceError(ctx, "this binding cannot be used out of a class")
}

class BooleanLiteral(ctx: SourceContext, val value: Boolean) : Literal(ctx) {
  override val type = MxBool
}

class NullLiteral(ctx: SourceContext) : Literal(ctx) {
  override val type = MxTop
}

class LambdaExpression(
  ctx: SourceContext,
  val params: List<FunctionParam>,
  val body: BlockStatement,
) : Node(ctx), Expression {
  override val type: Type
    get() {
      val ret = body.type
      if (!ret.complete) throw TypeError(ctx, "Cannot deduce return type")
      return MxFunction(params.map { it.typeId.type }, ret)
    }
}

class NewExpression(
  ctx: SourceContext,
  val typeId: NewTypeId,
) : Node(ctx), Expression {
  override val type = typeId.type
}

class MemberExpression(
  ctx: SourceContext,
  val `object`: Expression,
  val prop: Identifier,
) : Node(ctx), Expression {
  override val type = `object`.type
}

sealed interface TypeId {
  val type: Type
}
sealed interface NewTypeId {
  val type: Type
}
