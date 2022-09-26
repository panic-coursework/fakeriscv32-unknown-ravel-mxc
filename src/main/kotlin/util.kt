package org.altk.lab.mxc

import org.antlr.v4.runtime.*

data class Position(val line: Int, val col: Int) {
  override fun toString() = "$line:$col"
}

data class SourceLocation(val start: Position, val end: Position)

open class SourceContext(val parsed: ParserRuleContext) {
  val loc: SourceLocation
    get() = SourceLocation(
      Position(parsed.start.line, parsed.start.charPositionInLine),
      endPosition(parsed.stop),
    )
  val source: String
    get() = parsed.text
}

private fun endPosition(token: Token): Position {
  val lines = token.text.split('\n')
  return if (lines.size == 1) {
    Position(token.line, token.charPositionInLine + token.text.length)
  } else {
    Position(token.line + lines.size - 1, lines.last().length)
  }
}
