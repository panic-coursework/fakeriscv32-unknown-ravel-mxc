package org.altk.lab.mxc.type

import org.altk.lab.mxc.ast.BinaryOperator
import org.altk.lab.mxc.ast.BinaryOperator.*
import org.altk.lab.mxc.ast.BuiltinSourceContext
import org.altk.lab.mxc.ast.UnaryOperator
import org.altk.lab.mxc.ast.UnaryOperator.*

private fun EnvironmentRecord.registerBuiltin(
  name: String,
  params: List<Type>,
  returnType: Type,
): EnvironmentRecord {
  val binding = Binding(
    BuiltinSourceContext,
    name,
    MxFunction(params, returnType),
    Mutability.IMMUTABLE,
  )
  createBinding(binding)
  return this
}

fun createStringEnv() = EnvironmentRecord(null)
  .registerBuiltin("length", listOf(), MxInt)
  .registerBuiltin("substring", listOf(MxInt, MxInt), MxString)
  .registerBuiltin("parseInt", listOf(), MxInt)
  .registerBuiltin("ord", listOf(MxInt), MxInt)

fun createArrayEnv() = EnvironmentRecord(null)
  .registerBuiltin("size", listOf(), MxInt)

fun createGlobalEnv() = GlobalEnvironmentRecord()
  .registerBuiltin("print", listOf(MxString), MxVoid)
  .registerBuiltin("println", listOf(MxString), MxVoid)
  .registerBuiltin("printInt", listOf(MxInt), MxVoid)
  .registerBuiltin("printlnInt", listOf(MxInt), MxVoid)
  .registerBuiltin("getString", listOf(), MxString)
  .registerBuiltin("getInt", listOf(), MxInt)
  .registerBuiltin("toString", listOf(MxInt), MxString)

val BinaryOperator.operandTypes
  get() = when (this) {
    LE, GE, LT, GT, EQ, NE -> setOf(MxInt, MxString, MxBool)
    ADD -> setOf(MxInt, MxString)
    SUB, MUL, DIV, REM, SHL, SHR, BIT_AND, BIT_OR, BIT_XOR -> setOf(MxInt)
    AND, OR -> setOf(MxBool)
  }

fun BinaryOperator.resultType(operandType: Type) = when (this) {
  LE, GE, LT, GT, EQ, NE, AND, OR -> MxBool
  ADD, SUB, MUL, DIV, REM, SHL, SHR, BIT_AND, BIT_OR, BIT_XOR -> operandType
}

val BinaryOperator.isEquivalenceOp
  get() = when (this) {
    LE, GE, LT, GT, EQ, NE -> true
    ADD, SUB, MUL, DIV, REM, SHL, SHR, AND, OR, BIT_AND, BIT_OR, BIT_XOR -> false
  }

val UnaryOperator.operandType
  get() = when (this) {
    NOT -> MxBool
    BIT_NOT, POS, NEG -> MxInt
  }
