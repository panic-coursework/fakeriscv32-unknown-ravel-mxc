package org.altk.lab.mxc.ir

import org.altk.lab.mxc.type.*

private fun EnvironmentRecord.functionDeclarations(prefix: String) = bindings
  .map { entry ->
    (entry.value.type as? MxFunction)?.let { ty ->
      FunctionDeclaration(
        GlobalNamedIdentifier(prefix + entry.key),
        ty.params.map { it.ir },
        ty.returnType.ir
      )
    }
  }.filterNotNull().toTypedArray()

val prelude = listOf(
  *createGlobalEnv().functionDeclarations(""),
  *createStringEnv().functionDeclarations("string."),
  *createArrayEnv().functionDeclarations("array."),

  // helper functions
  FunctionDeclaration(
    GlobalNamedIdentifier("string.+"),
    listOf(MxStringType, MxStringType),
    MxStringType,
  ),
  FunctionDeclaration(
    GlobalNamedIdentifier("string.=="),
    listOf(MxStringType, MxStringType),
    BoolType,
  ),

  // libc functions
  FunctionDeclaration(
    GlobalNamedIdentifier("malloc"),
    listOf(Int32Type),
    PointerType(null),
  ),
)
