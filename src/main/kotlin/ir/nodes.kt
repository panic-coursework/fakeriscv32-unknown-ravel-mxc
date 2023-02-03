package org.altk.lab.mxc.ir

import org.altk.lab.mxc.MxcInternalError

sealed interface Node {
  val text: String
}


class Comment(val content: String) : Node {
  override val text = content.split('\n').joinToString("\n") { "; $it" }
}

sealed interface Operand : Node

sealed interface Identifier : Operand {
  val name: String
  val escapedName get() = escapeIdentifier(name)
}

class IntegerLiteral(val value: Int) : Operand {
  override val text get() = value.toString()
}

class StringLiteral(val content: ByteArray) : Operand {
  override val text
    get() = "[ ${(content + 0).joinToString(", ") { "i8 $it" }} ]"
  val value get() = Value(this, ArrayType(CharType, content.size + 1))
}

object NullLiteral : Operand {
  override val text get() = "null"
}

class AggregateLiteral(val values: List<Value<*>>) :
  Operand {
  override val text
    get() = "{ ${values.joinToString(", ") { it.text }} }"
}

object Undef : Operand {
  override val text get() = "undef"
}

sealed interface Type : Node

sealed interface GlobalIdentifier : Identifier {
  override val text get() = "@${escapedName}"
}

sealed interface LocalIdentifier : Identifier {
  override val text get() = "%${escapedName}"
}

sealed class NamedIdentifier(override val name: String) : Identifier {
  override fun toString() = "NamedIdentifier($name)"
  override fun hashCode() = name.hashCode()
  override fun equals(other: Any?) =
    other is NamedIdentifier &&
      this is LocalIdentifier == other is LocalIdentifier &&
      this is GlobalIdentifier == other is GlobalIdentifier &&
      name == other.name
}

sealed class UnnamedIdentifier(val id: Int) : Identifier {
  override val name get() = id.toString()
  override val escapedName get() = name
  override fun hashCode() = id
  override fun equals(other: Any?) =
    other is UnnamedIdentifier &&
      this is LocalIdentifier == other is LocalIdentifier &&
      this is GlobalIdentifier == other is GlobalIdentifier &&
      id == other.id
}

class GlobalNamedIdentifier(name: String) :
  GlobalIdentifier, NamedIdentifier(name)

class LocalNamedIdentifier(name: String) :
  LocalIdentifier, NamedIdentifier(name), Type

class GlobalUnnamedIdentifier(id: Int) : GlobalIdentifier, UnnamedIdentifier(id)
class LocalUnnamedIdentifier(id: Int) : LocalIdentifier, UnnamedIdentifier(id)

open class Value<out T : Type>(val operand: Operand, val type: T) : Node {
  override val text get() = "${type.text} ${operand.text}"
  inline fun <reified U : Type> asType(): Value<U> {
    if (type !is U) {
      throw MxcInternalError(null, "wrong ir operand type, got $type")
    }
    return Value(operand, type)
  }

  override fun toString() = "Value($operand, $type)"
}

fun valueOf(operand: Operand, type: Type): Value<*> = when (type) {
  is AggregateType -> Value(operand, type)
  is FunctionType -> Value(operand, type)
  is LocalNamedIdentifier -> Value(operand, type)
  BoolType -> Value(operand, type)
  CharType -> Value(operand, type)
  Int32Type -> Value(operand, type)
  LabelType -> Value(operand, type)
  is PointerType -> Value(operand, type)
  VoidType -> Value(operand, type)
  is ArrayType -> Value(operand, type)
}


sealed class PrimitiveType(val name: String) : Type {
  override val text get() = name
}

sealed interface DereferencableType : Type {
  fun deref(index: Int): Type
}

sealed interface ComparableType : Type
sealed class IntType(val length: Int) : PrimitiveType("i$length"),
  ComparableType

object Int32Type : IntType(32)
object CharType : IntType(8)
object BoolType : IntType(1)
object VoidType : PrimitiveType("void")
object LabelType : PrimitiveType("label")
open class PointerType(val pointee: Type?) : PrimitiveType("ptr"),
  DereferencableType, ComparableType {
  override fun deref(index: Int) = pointee!!
  override fun toString() = "Pointer($pointee)"
}

open class ArrayType(val content: Type, val length: Int) : DereferencableType {
  override val text = "[ $length x ${content.text} ]"
  override fun deref(index: Int) = content
  override fun toString() = "ArrayType($content, $length)"
}

open class AggregateType(val subtypes: List<Type>) : Type, DereferencableType {
  override val text get() = "{ ${subtypes.joinToString(", ") { it.text }} }"
  override fun deref(index: Int) = subtypes[index]
  override fun toString() = "AggregateType(${subtypes.joinToString(", ")})"
}

class MxStructType(
  val id: LocalNamedIdentifier,
  subtypes: List<Type>,
  val indexFromName: Map<String, Int>,
) : PointerType(AggregateType(subtypes))

class FunctionType(val args: List<Type>, val returnType: Type) : Type {
  override val text
    get() = "${returnType.text} (${args.joinToString(", ") { it.text }})"
}

open class MxArrayType(val content: Type) : PointerType(ArrayType(content, 0))

object MxStringType : MxArrayType(CharType)

class MxClosureType(val args: List<Type>, val returnType: Type) :
  AggregateType(
    // TODO: capture type
    listOf(PointerType(null), PointerType(FunctionType(args, returnType)))
  )

class Label(id: LocalIdentifier) : Value<LabelType>(id, LabelType)


sealed interface ModuleItem : Node
sealed interface Instruction : Node {
  fun replace(x: LocalIdentifier, y: Operand): Instruction
  val uses: List<LocalIdentifier>
}

private fun <T : Type> Value<T>.r(x: LocalIdentifier, y: Operand) =
  if (operand == x) Value(y, type) else this

private val Value<*>.u get() = operand.u

private fun Label.r(x: LocalIdentifier, y: Operand) =
  if (operand == x) Label(y as LocalIdentifier) else this

private fun Operand.r(x: LocalIdentifier, y: Operand) =
  if (this == x) y else this

private val Operand.u
  get() = if (this is LocalIdentifier) listOf(this) else listOf()

private fun LocalIdentifier.r(x: LocalIdentifier, y: Operand) =
  if (this == x) y else this

private fun LocalIdentifier.rI(x: LocalIdentifier, y: Operand) =
  if (this == x) y as LocalIdentifier else this


class Module(val body: List<ModuleItem>) : Node {
  override val text = body.joinToString("\n\n") { it.text }
}

class GlobalVariableDeclaration(
  val id: GlobalIdentifier,
  val value: Value<*>,
) : ModuleItem {
  override val text get() = "${id.text} = global ${value.text}"
}

class TypeDeclaration(
  val id: LocalNamedIdentifier,
  val type: Type,
) : ModuleItem {
  override val text get() = "${id.text} = type ${type.text}"
}

class FunctionDeclaration(
  val id: GlobalNamedIdentifier,
  val args: List<Type>,
  val returnType: Type,
) : ModuleItem {
  override val text
    get() = "declare ${returnType.text} ${id.text} (${args.joinToString(", ") { it.text }})"
}

class BasicBlock(
  val label: Label,
  val body: List<Instruction>,
  val estimatedFrequency: Int,
) : Node {
  override val text get() = "${(label.operand as Identifier).escapedName}:\n${bodyText}"
  private val bodyText get() = indent(body.joinToString("\n") { it.text })
  val successors get() = (body.last() as TerminatorInstruction).successors
}

class FunctionDefinition(
  val id: GlobalNamedIdentifier,
  val args: List<Value<*>>,
  val returnType: Type,
  val body: List<BasicBlock>,
) : ModuleItem {
  override val text
    get() = "define ${returnType.text} ${id.text} (${args.joinToString(", ") { it.text }}) {\n${bodyText}\n}"
  private val bodyText get() = body.joinToString("\n\n") { it.text }
}

sealed interface TerminatorInstruction : Instruction {
  val successors: Set<Label>
}

class ReturnValue(val value: Value<*>) : TerminatorInstruction {
  override val successors get() = setOf<Label>()
  override val text get() = "ret ${value.text}"
  override val uses get() = value.u
  override fun replace(x: LocalIdentifier, y: Operand) =
    ReturnValue(value.r(x, y))
}

object ReturnVoid : TerminatorInstruction {
  override val successors get() = setOf<Label>()
  override val text = "ret void"
  override val uses = listOf<LocalIdentifier>()
  override fun replace(x: LocalIdentifier, y: Operand) = this
}

class BranchConditional(
  val condition: Value<BoolType>,
  val consequent: Label,
  val alternate: Label,
) : TerminatorInstruction {
  override val successors get() = setOf(consequent, alternate)
  override val text get() = "br ${condition.text}, ${consequent.text}, ${alternate.text}"
  override val uses get() = condition.u
  override fun replace(x: LocalIdentifier, y: Operand) =
    BranchConditional(condition.r(x, y), consequent.r(x, y), alternate.r(x, y))
}

class BranchUnconditional(val dest: Label) : TerminatorInstruction {
  override val successors get() = setOf(dest)
  override val text get() = "br ${dest.text}"
  override val uses get() = listOf<LocalIdentifier>()
  override fun replace(x: LocalIdentifier, y: Operand) =
    BranchUnconditional(dest.r(x, y))
}

object Unreachable : TerminatorInstruction {
  override val successors get() = setOf<Label>()
  override val text = "unreachable"
  override val uses = listOf<LocalIdentifier>()
  override fun replace(x: LocalIdentifier, y: Operand) = this
}

sealed class Operation(val result: LocalIdentifier, val type: Type) :
  Instruction {
  val value: Value<*> get() = valueOf(result, type)
}

class Move(val src: Value<*>, val dest: LocalIdentifier) :
  Operation(dest, src.type) {
  override val text get() = "${dest.text} = ${src.text}"
  override val uses get() = src.u
  override fun replace(x: LocalIdentifier, y: Operand) =
    Move(src.r(x, y), dest.rI(x, y))
}

sealed class BinaryOperation(
  result: LocalIdentifier,
  open val lhs: Value<*>,
  open val rhs: Value<*>,
  val op: String,
  open val operandType: Type,
  open val resultType: Type = operandType,
) : Operation(result, resultType) {
  override val text
    get() = "${result.text} = $op ${operandType.text} ${lhs.operand.text}, ${rhs.operand.text}"

  override val uses get() = lhs.u + rhs.u
}

class Int32BinaryOperation(
  result: LocalIdentifier,
  val opType: Op,
  override val lhs: Value<Int32Type>,
  override val rhs: Value<Int32Type>,
) : BinaryOperation(result, lhs, rhs, opType.toString(), Int32Type) {
  enum class Op {
    ADD, SUB, MUL, SDIV, SREM, SHL, ASHR;
    // UDIV, UREM, LSHR

    override fun toString() = name.lowercase()
  }

  override fun replace(x: LocalIdentifier, y: Operand) =
    Int32BinaryOperation(result.rI(x, y), opType, lhs.r(x, y), rhs.r(x, y))
}

class IntBinaryOperation(
  result: LocalIdentifier,
  override val resultType: IntType,
  val opType: Op,
  override val lhs: Value<IntType>,
  override val rhs: Value<IntType>,
) : BinaryOperation(result, lhs, rhs, opType.toString(), resultType) {
  enum class Op {
    OR, AND, XOR;

    override fun toString() = when (this) {
      OR -> "or"
      AND -> "and"
      XOR -> "xor"
    }
  }

  override fun replace(x: LocalIdentifier, y: Operand) =
    IntBinaryOperation(
      result.rI(x, y),
      resultType,
      opType,
      lhs.r(x, y),
      rhs.r(x, y),
    )
}

class Icmp(
  result: LocalIdentifier,
  val cond: Condition,
  val paramType: ComparableType,
  override val lhs: Value<ComparableType>,
  override val rhs: Value<ComparableType>,
) : BinaryOperation(result, lhs, rhs, "icmp $cond", paramType, BoolType) {
  enum class Condition {
    EQ, NE, SGT, SGE, SLT, SLE;
    // UGT, UGE, ULT, ULE

    override fun toString() = name.lowercase()
  }

  override fun replace(x: LocalIdentifier, y: Operand) =
    Icmp(result.rI(x, y), cond, paramType, lhs.r(x, y), rhs.r(x, y))
}

class Phi(result: LocalIdentifier, val cases: List<Case>) :
  Operation(result, cases[0].value.type) {
  data class Case(val value: Value<*>, val condition: Label) {
    override fun toString() =
      "[ ${value.operand.text}, ${condition.operand.text} ]"
  }

  override val uses get() = cases.flatMap { it.value.u }
  override val text
    get() = "${result.text} = phi ${type.text} ${cases.joinToString(", ")}"

  override fun replace(x: LocalIdentifier, y: Operand) =
    Phi(
      result.rI(x, y),
      cases.map { Case(it.value.r(x, y), it.condition.r(x, y)) },
    )
}


sealed interface GenericCall : Instruction {
  val function: Value<FunctionType>
  val args: List<Value<*>>
}

class Call(
  result: LocalIdentifier,
  override val function: Value<FunctionType>,
  override val args: List<Value<*>>
) : Operation(result, function.type.returnType), GenericCall {
  private val argsText get() = args.joinToString(", ") { it.text }
  override val text: String
    get() = "${result.text} = tail call ${type.text} ${function.operand.text}(${argsText})"

  init {
    val actual = args.map { it.type }
    val expected = function.type.args
    val typeError = actual.size != expected.size
    if (typeError) {
      throw MxcInternalError(
        null,
        "call instruction type mismatch, got $actual, expecting $expected",
      )
    }
  }

  override val uses get() = args.flatMap { it.u }
  override fun replace(x: LocalIdentifier, y: Operand) =
    Call(result.rI(x, y), function.r(x, y), args.map { it.r(x, y) })
}

class CallVoid(
  override val function: Value<FunctionType>,
  override val args: List<Value<*>>,
) : Instruction, GenericCall {
  private val argsText get() = args.joinToString(", ") { it.text }
  override val text: String
    get() = "call void ${function.operand.text}(${argsText})"

  init {
    val actual = args.map { it.type }
    val expected = function.type.args
    val typeError = actual.size != expected.size
    if (typeError) {
      throw MxcInternalError(
        null,
        "call instruction type mismatch, got $actual, expecting $expected",
      )
    }
  }

  override val uses get() = listOf<LocalIdentifier>()
  override fun replace(x: LocalIdentifier, y: Operand) =
    CallVoid(function.r(x, y), args.map { it.r(x, y) })
}


class ExtractValue(
  result: LocalIdentifier,
  val target: Value<AggregateType>,
  val index: Int,
) : Operation(result, target.type.subtypes[index]) {
  override val text
    get() = "${result.text} = extractvalue ${target.text}, $index"

  override val uses get() = target.u
  override fun replace(x: LocalIdentifier, y: Operand) =
    ExtractValue(result.rI(x, y), target.r(x, y), index)
}

class InsertValue(
  result: LocalIdentifier,
  val target: Value<AggregateType>,
  val rhs: Value<*>,
  val index: Int,
) : Operation(result, target.type) {
  override val text
    get() = "${result.text} = insertvalue ${target.text}, ${rhs.text}, $index"

  override val uses get() = target.u + rhs.u
  override fun replace(x: LocalIdentifier, y: Operand) =
    InsertValue(result.rI(x, y), target.r(x, y), rhs.r(x, y), index)
}


class Alloca(result: LocalIdentifier, val content: Type) :
  Operation(result, PointerType(content)) {
  override val text get() = "${result.text} = alloca ${content.text}"
  override val uses get() = listOf<LocalIdentifier>()
  override fun replace(x: LocalIdentifier, y: Operand) =
    Alloca(result.rI(x, y), content)
}

class Load(result: LocalIdentifier, val target: Value<PointerType>) :
  Operation(result, target.type.pointee!!) {
  override val text
    get() = "${result.text} = load ${target.type.pointee!!.text}, ptr ${target.operand.text}"

  override val uses get() = target.u
  override fun replace(x: LocalIdentifier, y: Operand) =
    Load(result.rI(x, y), target.r(x, y))
}

class Store(val target: Operand, val content: Value<*>) : Instruction {
  override val text get() = "store ${content.text}, ptr ${target.text}"
  override val uses get() = target.u + content.u
  override fun replace(x: LocalIdentifier, y: Operand) =
    Store(target.r(x, y), content.r(x, y))
}

sealed interface GepIndex {
  val text: String
}

data class GepIndexLiteral(val index: Int) : GepIndex {
  override val text get() = "i32 $index"
}

data class GepIndexValue(val index: Value<Int32Type>) : GepIndex {
  override val text get() = index.text
}

class GetElementPtr(
  result: LocalIdentifier,
  val target: Value<PointerType>,
  val indices: List<GepIndex>
) : Operation(
  result,
  PointerType(indices.fold<GepIndex, Type>(target.type) { ty, i ->
    when (i) {
      is GepIndexLiteral -> (ty as DereferencableType).deref(i.index)
      is GepIndexValue -> when (ty) {
        is PointerType -> ty.pointee!!
        is ArrayType -> ty.content
        else -> throw MxcInternalError(
          null,
          "Unexpected $ty at GEP (target ${target.type})",
        )
      }
    }
  })
) {
  private val derefedType = target.type.deref(0)
  override val text
    get() =
      "${result.text} = getelementptr inbounds ${derefedType.text}, ptr ${target.operand.text}, ${
        indices.joinToString(", ") { it.text }
      }"

  override val uses
    get() = target.u +
      indices.filterIsInstance<GepIndexValue>().flatMap { it.index.u }

  override fun replace(x: LocalIdentifier, y: Operand) =
    GetElementPtr(result.rI(x, y), target.r(x, y), indices.map {
      when (it) {
        is GepIndexLiteral -> it
        is GepIndexValue -> GepIndexValue(it.index.r(x, y))
      }
    })
}
