package org.altk.lab.mxc

open class MxcError(val ctx: SourceContext?, msg: String) : Exception(msg) {
  override fun toString(): String {
    val msg = super.toString()
    if (ctx == null) return msg
    return "In $ctx: $msg"
  }

  fun print(sourceLines: List<String>) {
    if (ctx != null) System.err.println(ctx.format(sourceLines))
    printStackTrace(System.err)
  }
}

open class MxcInternalError(ctx: SourceContext?, msg: String) :
  MxcError(ctx, msg)

open class SyntaxError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class ReferenceError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class TypeError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
