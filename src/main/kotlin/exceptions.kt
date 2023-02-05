package org.altk.lab.mxc

import org.altk.lab.mxc.ast.SourceContext

open class MxcError(val ctx: SourceContext?, msg: String) : Exception(msg) {
  override fun toString(): String {
    val className = this::class.simpleName
    val msg = "$className: $message"
    if (ctx == null) return msg
    return "In ${ctx.loc}: $msg"
  }

  fun print() {
    if (ctx != null) System.err.println(ctx.toString())
    printStackTrace(System.err)
  }
}

open class MxcInternalError(ctx: SourceContext?, msg: String) :
  MxcError(ctx, msg)

open class NotImplemented(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class SyntaxError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class ReferenceError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class TypeError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
