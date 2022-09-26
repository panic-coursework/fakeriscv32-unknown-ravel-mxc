package org.altk.lab.mxc

open class MxcError(val ctx: SourceContext?, msg: String) : Exception(msg) {
  override fun toString(): String {
    val msg = super.toString()
    if (ctx == null) return msg
    return "In input:${ctx.loc.start} to input:${ctx.loc.end} : $msg\n${ctx.source}"
  }
}

open class MxcInternalError(ctx: SourceContext?, msg: String) :
  MxcError(ctx, msg)

open class SyntaxError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class ReferenceError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class TypeError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
