package org.altk.lab.mxc.ast

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token

open class Source(val filename: String?, val sourceText: String) {
  val lines = sourceText.split('\n')
  override fun toString() = filename ?: "[unknown]"
}

object BuiltinSource : Source("[native code]", "[native code]")

data class Position(val source: Source, val line: Int, val col: Int) {
  override fun toString() = "$source:$line:$col"
}

open class SourceLocation(val start: Position, val end: Position) {
  override fun toString() = start.toString()
}

object BuiltinSourceLocation :
  SourceLocation(Position(BuiltinSource, 0, 0), Position(BuiltinSource, 0, 0)) {
  override fun toString() = "[native code]"
}

@Suppress("MemberVisibilityCanBePrivate")
open class SourceContext(val source: Source, val parsed: ParserRuleContext?) {
  open val loc: SourceLocation
    get() = SourceLocation(
      Position(source, parsed!!.start.line, parsed.start.charPositionInLine),
      endPosition(source, parsed.stop),
    )

  override fun toString(): String {
    val alignment = loc.end.line.toString().length + 1
    val spacer = " | "
    val hintHeading = " ".repeat(alignment) + spacer
    if (loc.start.line == loc.end.line) {
      val text = source.lines[loc.start.line - 1].replace('\t', ' ')
      val hint =
        " ".repeat(loc.start.col) + "^".repeat(loc.end.col - loc.start.col)
      return " ${loc.end.line} | $text\n$hintHeading$hint"
    }
    val builder = StringBuilder()
    for (line in loc.start.line..loc.end.line) {
      val text = source.lines[line - 1].replace('\t', ' ')
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
}

object BuiltinSourceContext : SourceContext(BuiltinSource, null) {
  override val loc = BuiltinSourceLocation
  override fun toString() = "[native code]"
}

private fun endPosition(source: Source, token: Token): Position {
  val lines = token.text.split('\n')
  return if (lines.size == 1) {
    Position(source, token.line, token.charPositionInLine + token.text.length)
  } else {
    Position(source, token.line + lines.size - 1, lines.last().length)
  }
}
