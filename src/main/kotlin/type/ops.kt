package org.altk.lab.mxc.type

infix fun Type.intersect(other: Type): Type {
  if (this !is IncompleteType && other is IncompleteType) {
    return other intersect this
  }
  if (this is MxBot) return MxBot
  if (this is MxTop || this is MxHole) return other
  if (this is MxNullptr) return if (other is PrimitiveType) MxBot else other
  if (this == other) return this

  when (this) {
    is MxArray -> {
      if (other !is MxArray) return MxBot
      return MxArray(this.content intersect other.content)
    }

    is MxFunction -> {
      if (other !is MxFunction) return MxBot
      if (this.params.size != other.params.size) return MxBot
      // usually function parameters are not treated this way, but since we have
      // no subtyping in this type system, so this is fine.
      val params = this.params.zip(other.params).map { (a, b) -> a intersect b }
      val returnType = this.returnType intersect other.returnType
      return MxFunction(params, returnType)
    }

    else -> return MxBot
  }
}

infix fun Type.union(other: Type): Type {
  if (this !is IncompleteType && other is IncompleteType) {
    return other union this
  }
  if (this is MxTop || other is MxTop) return MxTop
  if (this is MxHole || this is MxBot) return other
  if (this is MxNullptr) return if (other is PrimitiveType) MxTop else other
  if (this == other) return this

  when (this) {
    is MxArray -> {
      if (other !is MxArray) return MxTop
      return MxArray(this.content union other.content)
    }

    is MxFunction -> {
      if (other !is MxFunction) return MxTop
      if (this.params.size != other.params.size) return MxTop
      val params = this.params.zip(other.params).map { (a, b) -> a union b }
      val returnType = this.returnType union other.returnType
      return MxFunction(params, returnType)
    }

    else -> return MxTop
  }
}

infix fun Type.leq(other: Type) = (this intersect other) == this
