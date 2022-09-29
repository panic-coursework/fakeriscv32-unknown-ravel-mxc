package org.altk.lab.mxc

import org.antlr.v4.runtime.*

data class Position(val line: Int, val col: Int) {
  override fun toString() = "$line:$col"
}

open class SourceLocation(val start: Position, val end: Position) {
  override fun toString() = "input:$start to input:$end"
}

class BuiltinSourceLocation : SourceLocation(Position(-1, -1), Position(-1, -1)) {
  override fun toString() = "[native code]"
}

open class SourceContext(val parsed: ParserRuleContext?) {
  open val loc: SourceLocation
    get() = SourceLocation(
      Position(parsed!!.start.line, parsed.start.charPositionInLine),
      endPosition(parsed.stop),
    )
  open val source: String
    get() = parsed!!.text

  override fun toString() = loc.toString()
}

object BuiltinSourceContext : SourceContext(null) {
  override val loc = BuiltinSourceLocation()
  override val source = "[native code]"
}

private fun endPosition(token: Token): Position {
  val lines = token.text.split('\n')
  return if (lines.size == 1) {
    Position(token.line, token.charPositionInLine + token.text.length)
  } else {
    Position(token.line + lines.size - 1, lines.last().length)
  }
}
