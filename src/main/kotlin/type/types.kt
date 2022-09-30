package org.altk.lab.mxc.type

sealed class Type {
  open val typeArgs: List<Type> = listOf()

  open val complete: Boolean
    get() = typeArgs.all { it.complete }
  open val isVoid get() = false

  override fun equals(other: Any?): Boolean {
    if (other == null) return false
    if (other::class != this::class || other !is Type) return false
    return typeArgs == other.typeArgs
  }

  override fun hashCode() = typeArgs.hashCode()
}

sealed class PrimitiveType(val name: String) : Type() {
  override fun toString() = name
}

sealed class IncompleteType(val name: String) : Type() {
  override val complete = false
  override fun toString() = name
}

sealed interface ObjectType {
  val env: EnvironmentRecord
}

object MxTop : IncompleteType("top")
object MxHole : IncompleteType("_")
object MxBot : IncompleteType("bot")
object MxType : IncompleteType("type")
object MxNullptr : IncompleteType("null")

object MxInt : PrimitiveType("int")
object MxBool : PrimitiveType("bool")
object MxVoid : PrimitiveType("void") {
  override val isVoid get() = true
}
object MxString : PrimitiveType("string"), ObjectType {
  override val env = createStringEnv()
}

class MxArray(val content: Type) : Type(), ObjectType {
  override val typeArgs = listOf(content)
  override val env = createArrayEnv()
  override fun toString() = "($content)[]"
  override val isVoid get() = content.isVoid
}

class MxStruct(val name: String, override val env: EnvironmentRecord) : Type(),
  ObjectType {
  override fun equals(other: Any?) = this === other
  override fun hashCode() = super.hashCode()
  override fun toString() = name
}

class MxFunction(val params: List<Type>, val returnType: Type) : Type() {
  override val typeArgs = params + returnType
  override fun toString() = "(${params.joinToString(", ")}) -> $returnType"
}
