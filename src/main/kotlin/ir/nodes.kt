package org.altk.lab.mxc.ir

import org.altk.lab.mxc.MxcInternalError
import org.altk.lab.mxc.type.*
import org.altk.lab.mxc.type.Type as AstType

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

class StringLiteral(val value: String) : Operand {
  override val text
    get() = "[ ${
      value.split("").joinToString(", ") { "i8 ${it.codePointAt(0)}" }
    } ]"
}

class Undef : Operand {
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
  override fun hashCode() = name.hashCode()
  override fun equals(other: Any?) =
    other is NamedIdentifier && name == other.name
}

sealed class UnnamedIdentifier(val id: Int) : Identifier {
  override val name get() = id.toString()
  override val escapedName get() = name
  override fun hashCode() = id
  override fun equals(other: Any?) =
    other is UnnamedIdentifier && id == other.id
}

class GlobalNamedIdentifier(name: String) :
  GlobalIdentifier, NamedIdentifier(name)

class LocalNamedIdentifier(name: String) :
  LocalIdentifier, NamedIdentifier(name), Type

class GlobalUnnamedIdentifier(id: Int) : GlobalIdentifier, UnnamedIdentifier(id)
class LocalUnnamedIdentifier(id: Int) : LocalIdentifier, UnnamedIdentifier(id)

open class Value<out T : Type>(val operand: Operand, val type: T) : Node {
  override val text get() = "${type.text} ${operand.text}"
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

sealed class IntType(val length: Int) : PrimitiveType("i$length")
object Int32Type : IntType(32)
object CharType : IntType(8)
object BoolType : IntType(1)
object VoidType : PrimitiveType("void")
object LabelType : PrimitiveType("label")
open class PointerType(val pointee: Type?) : PrimitiveType("ptr"),
  DereferencableType {
  override fun deref(index: Int) = pointee!!
}

open class ArrayType(val content: Type, val length: Int) : DereferencableType {
  override val text = "[ $length x ${content.text} ]"
  override fun deref(index: Int) = content
}

open class AggregateType(val subtypes: List<Type>) : Type, DereferencableType {
  override val text get() = "{ ${subtypes.joinToString(", ") { it.text }} }"
  override fun deref(index: Int) = subtypes[index]
}

class FunctionType(val params: List<Type>, val returnType: Type) : Type {
  override val text
    get() = "${returnType.text} (${params.joinToString(", ") { it.text }})"
}

class MxArrayType(val content: Type) :
  AggregateType(listOf(Int32Type, PointerType(content)))

object MxStringType : AggregateType(listOf(Int32Type, PointerType(CharType)))

class MxClosureType(val params: List<Type>, val returnType: Type) :
  AggregateType(
    // TODO: capture type
    listOf(PointerType(null), PointerType(FunctionType(params, returnType)))
  )

class Label(id: LocalNamedIdentifier) : Value<LabelType>(id, LabelType)


val AstType.ir: Type
  get() = when (this) {
    MxInt -> Int32Type
    MxNullptr -> PointerType(null)
    MxString -> MxStringType
    is MxFunction -> MxClosureType(params.map { it.ir }, returnType.ir)
    is MxStruct -> PointerType(LocalNamedIdentifier("struct.$name"))
    is MxArray -> MxArrayType(content.ir)
    MxBool -> BoolType
    MxVoid -> VoidType
    MxBot, MxTop, MxHole, MxType ->
      throw MxcInternalError(null, "Unexpected type $this")
  }


sealed interface ModuleItem : Node
sealed interface Instruction : Node

class Module(val body: List<ModuleItem>) : Node {
  override val text = body.joinToString("\n\n") { it.text }
}

class GlobalVariableDeclaration(
  val id: GlobalIdentifier,
  val type: Type,
) : ModuleItem {
  override val text get() = "${id.text} = global ${type.text}"
}

class TypeDeclaration(
  val id: LocalNamedIdentifier,
  val type: Type,
) : ModuleItem {
  override val text get() = "${id.text} = type ${type.text}"
}

class FunctionDeclaration(
  val id: GlobalNamedIdentifier,
  val params: List<Type>,
  val returnType: Type,
) : ModuleItem {
  override val text
    get() = "declare ${returnType.text} ${id.text} (${params.joinToString(", ") { it.text }})"
}

class BasicBlock(
  val label: LocalIdentifier,
  val body: List<Instruction>,
) : Node {
  override val text get() = "${label.text}:\n${bodyText}"
  private val bodyText get() = indent(body.joinToString("\n") { it.text })
}

class FunctionDefinition(
  val id: GlobalNamedIdentifier,
  val params: List<Type>,
  val returnType: Type,
  val body: List<BasicBlock>,
) : ModuleItem {
  override val text
    get() = "define ${returnType.text} ${id.text} (${params.joinToString(", ") { it.text }}) {\n${bodyText}\n}"
  private val bodyText get() = body.joinToString("\n\n") { it.text }
}

sealed interface TerminatorInstruction : Instruction

class ReturnValue(val value: Value<*>) : TerminatorInstruction {
  override val text get() = "ret ${value.text}"
}

object ReturnVoid : TerminatorInstruction {
  override val text = "ret void"
}

class BranchConditional(
  val condition: Value<BoolType>,
  val consequent: Label,
  val alternate: Label,
) : TerminatorInstruction {
  override val text get() = "br ${condition.text}, ${consequent.text}, ${alternate.text}"
}

class BranchUnconditional(val dest: Label) : TerminatorInstruction {
  override val text get() = "br ${dest.text}"
}

object Unreachable : TerminatorInstruction {
  override val text = "unreachable"
}

sealed class Operation(val result: LocalIdentifier, val type: Type) :
  Instruction {
  val value: Value<*> get() = valueOf(result, type)
}

sealed class BinaryOperation(
  result: LocalIdentifier,
  val lhs: Value<*>,
  val rhs: Value<*>,
  val op: String,
  type: Type,
) : Operation(result, type) {
  override val text
    get() = "${result.text} = $op ${type.text} ${lhs.operand.text}, ${rhs.operand.text}"
}

class Int32BinaryOperation(
  result: LocalIdentifier,
  val opType: Op,
  lhs: Value<Int32Type>,
  rhs: Value<Int32Type>,
) : BinaryOperation(result, lhs, rhs, opType.toString(), Int32Type) {
  enum class Op {
    ADD, SUB, MUL, UDIV, SDIV, UREM, SREM, SHL, LSHR, ASHR;

    override fun toString() = when (this) {
      ADD -> "add"
      SUB -> "sub"
      MUL -> "mul"
      UDIV -> "udiv"
      SDIV -> "sdiv"
      UREM -> "urem"
      SREM -> "srem"
      SHL -> "shl"
      LSHR -> "lshr"
      ASHR -> "ashr"
    }
  }
}

class IntBinaryOperation(
  result: LocalIdentifier,
  type: IntType,
  val opType: Op,
  lhs: Value<IntType>,
  rhs: Value<IntType>,
) : BinaryOperation(result, lhs, rhs, opType.toString(), type) {
  enum class Op {
    OR, AND, XOR;

    override fun toString() = when (this) {
      OR -> "or"
      AND -> "and"
      XOR -> "xor"
    }
  }
}

class Icmp(
  result: LocalIdentifier,
  val cond: Condition,
  type: IntType,
  lhs: Value<IntType>,
  rhs: Value<IntType>,
) : BinaryOperation(result, lhs, rhs, "icmp $cond", type) {
  enum class Condition {
    EQ, NE, UGT, UGE, ULT, ULE, SGT, SGE, SLT, SLE;

    override fun toString() = when (this) {
      EQ -> "eq"
      NE -> "ne"
      UGT -> "ugt"
      UGE -> "uge"
      ULT -> "ult"
      ULE -> "ule"
      SGT -> "sgt"
      SGE -> "sge"
      SLT -> "slt"
      SLE -> "sle"
    }
  }
}

class Phi(result: LocalIdentifier, val cases: List<Case>) :
  Operation(result, cases[0].value.type) {
  data class Case(val value: Value<*>, val condition: Label) {
    override fun toString() =
      "[ ${value.operand.text}, ${condition.operand.text} ]"
  }

  override val text
    get() = "${result.text} = phi ${type.text} ${cases.joinToString(", ")}"
}


class Call(
  result: LocalIdentifier,
  val function: Value<FunctionType>,
  val args: List<Value<*>>
) : Operation(result, function.type.returnType) {
  private val argsText get() = args.joinToString(", ") { it.text }
  override val text: String
    get() = "${result.text} = tail call ${type.text} ${function.operand.text}(${argsText})"

  init {
    val actual = args.map { it.type }
    val expected = function.type.params
    val typeError = actual.size != expected.size || actual.zip(expected)
      .any { it.first != it.second }
    if (typeError) {
      throw MxcInternalError(
        null,
        "call instruction type mismatch, got $actual, expecting $expected",
      )
    }
  }
}


class ExtractValue(
  result: LocalIdentifier,
  val target: Value<AggregateType>,
  val index: Int,
) : Operation(result, target.type.subtypes[index]) {
  override val text
    get() = "${result.text} = extractvalue ${target.text}, $index"
}

class InsertValue(
  result: LocalIdentifier,
  val target: Value<AggregateType>,
  val rhs: Value<*>,
  val index: Int,
) : Operation(result, target.type) {
  override val text
    get() = "${result.text} = insertvalue ${target.text}, ${rhs.text}, $index"
}


class Alloca(result: LocalIdentifier, type: Type) :
  Operation(result, PointerType(type)) {
  override val text get() = "${result.text} = alloca ${type.text}"
}

class Load(result: LocalIdentifier, val target: Value<PointerType>) :
  Operation(result, target.type.pointee!!) {
  override val text
    get() = "${result.text} = load ${target.type.pointee!!.text}, ptr ${target.operand.text}"
}

class Store(val target: Operand, val content: Value<*>) : Instruction {
  override val text get() = "store ${content.text}, ptr ${target.text}"
}

class GetElementPtr(
  result: LocalIdentifier,
  val target: Value<DereferencableType>,
  val indices: List<Int>
) : Operation(
  result,
  indices.fold<Int, Type>(target.type) { ty, i ->
    (ty as DereferencableType).deref(i)
  }) {
  override val text
    get() =
      "${result.text} = getelementptr inbounds ${target.type.text}, ptr ${target.operand.text}, ${
        indices.joinToString(", ") { "i32 $it" }
      }"
}
