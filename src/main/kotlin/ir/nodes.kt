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
    get() = "[ ${content.joinToString(", ") { "i8 $it" }}, i8 0 ]"
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
  inline fun <reified U : Type> asType(): Value<U> {
    if (type !is U) {
      throw MxcInternalError(null, "wrong ir operand type, got $type")
    }
    return Value(operand, type)
  }
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
}

open class ArrayType(val content: Type, val length: Int) : DereferencableType {
  override val text = "[ $length x ${content.text} ]"
  override fun deref(index: Int) = content
}

open class AggregateType(val subtypes: List<Type>) : Type, DereferencableType {
  override val text get() = "{ ${subtypes.joinToString(", ") { it.text }} }"
  override fun deref(index: Int) = subtypes[index]
}

class MxStructType(
  val id: LocalNamedIdentifier,
  subtypes: List<Type>,
  val indexFromName: Map<String, Int>,
) : PointerType(AggregateType(subtypes))

class FunctionType(val params: List<Type>, val returnType: Type) : Type {
  override val text
    get() = "${returnType.text} (${params.joinToString(", ") { it.text }})"
}

open class MxArrayType(val content: Type) :
  PointerType(AggregateType(listOf(Int32Type, ArrayType(content, 0))))

object MxStringType : MxArrayType(CharType)

class MxClosureType(val params: List<Type>, val returnType: Type) :
  AggregateType(
    // TODO: capture type
    listOf(PointerType(null), PointerType(FunctionType(params, returnType)))
  )

class Label(id: LocalIdentifier) : Value<LabelType>(id, LabelType)


sealed interface ModuleItem : Node
sealed interface Instruction : Node

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
  val params: List<Type>,
  val returnType: Type,
) : ModuleItem {
  override val text
    get() = "declare ${returnType.text} ${id.text} (${params.joinToString(", ") { it.text }})"
}

class BasicBlock(
  val label: Label,
  val body: List<Instruction>,
) : Node {
  override val text get() = "${(label.operand as Identifier).escapedName}:\n${bodyText}"
  private val bodyText get() = indent(body.joinToString("\n") { it.text })
}

class FunctionDefinition(
  val id: GlobalNamedIdentifier,
  val params: List<Value<*>>,
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
  val operandType: Type,
  val resultType: Type = operandType,
) : Operation(result, resultType) {
  override val text
    get() = "${result.text} = $op ${operandType.text} ${lhs.operand.text}, ${rhs.operand.text}"
}

class Int32BinaryOperation(
  result: LocalIdentifier,
  val opType: Op,
  lhs: Value<Int32Type>,
  rhs: Value<Int32Type>,
) : BinaryOperation(result, lhs, rhs, opType.toString(), Int32Type) {
  enum class Op {
    ADD, SUB, MUL, UDIV, SDIV, UREM, SREM, SHL, LSHR, ASHR;

    override fun toString() = name.lowercase()
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
  type: ComparableType,
  lhs: Value<ComparableType>,
  rhs: Value<ComparableType>,
) : BinaryOperation(result, lhs, rhs, "icmp $cond", type, BoolType) {
  enum class Condition {
    EQ, NE, UGT, UGE, ULT, ULE, SGT, SGE, SLT, SLE;

    override fun toString() = name.lowercase()
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

class CallVoid(val function: Value<FunctionType>, val args: List<Value<*>>) :
  Instruction {
  private val argsText get() = args.joinToString(", ") { it.text }
  override val text: String
    get() = "call void ${function.operand.text}(${argsText})"

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


class Alloca(result: LocalIdentifier, val content: Type) :
  Operation(result, PointerType(content)) {
  override val text get() = "${result.text} = alloca ${content.text}"
}

class Load(result: LocalIdentifier, val target: Value<PointerType>) :
  Operation(result, target.type.pointee!!) {
  override val text
    get() = "${result.text} = load ${target.type.pointee!!.text}, ptr ${target.operand.text}"
}

class Store(val target: Operand, val content: Value<*>) : Instruction {
  override val text get() = "store ${content.text}, ptr ${target.text}"
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
  val target: Value<DereferencableType>,
  val indices: List<GepIndex>
) : Operation(
  result,
  PointerType(indices.fold<GepIndex, Type>(target.type) { ty, i ->
    when (i) {
      is GepIndexLiteral -> (ty as DereferencableType).deref(i.index)
      is GepIndexValue -> (ty as PointerType).pointee!!
    }
  })
) {
  private val derefedType = target.type.deref(0)
  override val text
    get() =
      "${result.text} = getelementptr inbounds ${derefedType.text}, ptr ${target.operand.text}, ${
        indices.joinToString(", ") { it.text }
      }"
}
