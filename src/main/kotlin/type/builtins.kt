package org.altk.lab.mxc.type

import org.altk.lab.mxc.BuiltinSourceContext

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
