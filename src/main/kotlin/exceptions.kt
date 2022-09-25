package org.altk.lab.mxc

open class MxcError(val ctx: SourceContext?, msg: String) : Exception(msg)
open class ParseError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class ReferenceError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
open class TypeError(ctx: SourceContext?, msg: String) : MxcError(ctx, msg)
