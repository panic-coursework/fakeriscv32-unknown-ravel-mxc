package org.altk.lab.mxc.ir

import org.altk.lab.mxc.MxcInternalError
import org.altk.lab.mxc.type.*
import org.altk.lab.mxc.type.Type as AstType

private val AstType.ir
  get(): Type = when (this) {
    MxInt -> Int32Type
    MxNullptr -> PointerType(null)
    MxString -> MxStringType
    is MxArray -> MxArrayType(content.ir)
    MxBool -> BoolType
    MxVoid -> VoidType
    is MxFunction ->
      MxClosureType(params.map { it.ir }, returnType.ir)

    MxBot, MxTop, MxHole, MxType, is MxStruct ->
      throw MxcInternalError(null, "Unexpected type $this")
  }

private fun EnvironmentRecord.functionDeclarations(
  prefix: String,
  thisArg: Type? = null,
) = bindings.map { entry ->
  (entry.value.type as? MxFunction)?.let { ty ->
    FunctionDeclaration(
      GlobalNamedIdentifier(prefix + entry.key),
      (thisArg?.let { listOf(it) } ?: listOf()) + ty.params.map { it.ir },
      ty.returnType.ir
    )
  }
}.filterNotNull().toTypedArray()

val prelude = listOf(
  *createGlobalEnv().functionDeclarations(""),
  *createStringEnv().functionDeclarations("string.", PointerType(null)),
  *createArrayEnv().functionDeclarations("array.", PointerType(null)),

  // helper functions
  FunctionDeclaration(
    GlobalNamedIdentifier("string.+"),
    listOf(MxStringType, MxStringType),
    MxStringType,
  ),
  FunctionDeclaration(
    GlobalNamedIdentifier("string.<=>"),
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
