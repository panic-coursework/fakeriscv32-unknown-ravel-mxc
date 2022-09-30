package org.altk.lab.mxc

import org.antlr.v4.runtime.*

data class Position(val line: Int, val col: Int) {
  override fun toString() = "$line:$col"
}

open class SourceLocation(val start: Position, val end: Position) {
  override fun toString() = "input:$start"
}

class BuiltinSourceLocation :
  SourceLocation(Position(-1, -1), Position(-1, -1)) {
  override fun toString() = "[native code]"
}

open class SourceContext(val parsed: ParserRuleContext?) {
  open val loc: SourceLocation
    get() = SourceLocation(
      Position(parsed!!.start.line, parsed.start.charPositionInLine),
      endPosition(parsed.stop),
    )

  open fun format(sourceLines: List<String>): String {
    val alignment = loc.end.line.toString().length + 1
    val spacer = " | "
    val hintHeading = " ".repeat(alignment) + spacer
    if (loc.start.line == loc.end.line) {
      val text = sourceLines[loc.start.line - 1].replace('\t', ' ')
      val hint =
        " ".repeat(loc.start.col) + "^".repeat(loc.end.col - loc.start.col)
      return " ${loc.end.line} | $text\n$hintHeading$hint"
    }
    val builder = StringBuilder()
    for (line in loc.start.line..loc.end.line) {
      val text = sourceLines[line - 1].replace('\t', ' ')
      val hint = when (line) {
        loc.start.line -> " ".repeat(loc.start.col) + "^".repeat(text.length - loc.start.col)
        loc.end.line -> "^".repeat(loc.end.col)
        else -> "^".repeat(text.length)
      }
      builder.append(line.toString().padStart(alignment))
      builder.append(spacer)
      builder.append(text)
      builder.append('\n')
      builder.append(hintHeading)
      builder.append(hint)
      if (line != loc.end.line) builder.append('\n')
    }
    return builder.toString()
  }

  override fun toString() = loc.toString()
}

object BuiltinSourceContext : SourceContext(null) {
  override val loc = BuiltinSourceLocation()
  override fun format(sourceLines: List<String>) = "[native code]"
}

private fun endPosition(token: Token): Position {
  val lines = token.text.split('\n')
  return if (lines.size == 1) {
    Position(token.line, token.charPositionInLine + token.text.length)
  } else {
    Position(token.line + lines.size - 1, lines.last().length)
  }
}
