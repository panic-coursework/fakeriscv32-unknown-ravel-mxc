package org.altk.lab.mxc.typechecker

import org.altk.lab.mxc.*

interface Term {
  fun infer(): Type
  fun check(type: Type): Term
}

sealed class Type {
  open val typeArgs: List<Type> = listOf()

  open val complete: Boolean
    get() = typeArgs.all { it.complete }

  open fun intersect(ctx: SourceContext?, other: Type): Type {
    if (other is MxTop || other is MxTop) return this
    return if (other == this) this else MxBot
  }

  override fun equals(other: Any?): Boolean {
    if (other == null) return false
    if (other::class != this::class || other !is Type) return false
    return typeArgs == other.typeArgs
  }

  override fun hashCode() = typeArgs.hashCode()
}

sealed class PrimitiveType : Type()
sealed class IncompleteType : Type() {
  override val complete = false
}

object MxTop : IncompleteType() {
  override fun intersect(ctx: SourceContext?, other: Type) = MxTop
}
object MxBot : IncompleteType() {
  override fun intersect(ctx: SourceContext?, other: Type) = MxBot
}

object MxInt : PrimitiveType()
object MxBool : PrimitiveType()
object MxVoid : PrimitiveType()
object MxString : PrimitiveType()

class MxArray(val content: Type) : Type() {
  override val typeArgs = listOf(content)
  override fun intersect(ctx: SourceContext?, other: Type): Type {
    if (this == other || other is MxTop) return this
    if (other !is MxArray) return MxBot
    return MxArray(content.intersect(ctx, other.content))
  }
}

class MxStruct(val env: EnvironmentRecord) : Type() {
  override fun equals(other: Any?) = this === other
  override fun hashCode() = (Any::hashCode)(this)
}

class MxFunction(val params: List<Type>, val returnType: Type) : Type() {
  override val typeArgs = params + returnType
  override fun intersect(ctx: SourceContext?, other: Type): Type {
    if (this == other || other is MxTop) return this
    if (other !is MxFunction) return MxBot
    if (this.params.size != other.params.size) return MxBot
    // usually function parameters are not treated this way, but since we have
    // no subtyping in this type system, so this is fine.
    val params =
      this.params.zip(other.params).map { (a, b) -> a.intersect(ctx, b) }
    val returnType = this.returnType.intersect(ctx, other.returnType)
    return MxFunction(params, returnType)
  }
}
