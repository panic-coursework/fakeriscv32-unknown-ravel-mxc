package org.altk.lab.mxc.ast

import org.altk.lab.mxc.MxcInternalError
import org.altk.lab.mxc.recognizer.MxParser.*

fun parseInt(input: IntegerLiteralContext): Int {
  val text = input.text.replace("'", "")
  if (input is IntDecimalContext && text.startsWith("0d")) {
    return text.substring(2).toInt()
  }
  return when (input) {
    is IntDecimalContext -> text.toInt()
    is IntHexContext -> text.substring(2).toInt(0x10)
    is IntBinaryContext -> text.substring(2).toInt(0b10)
    else -> throw MxcInternalError(input.ctx, "Unable to parse int ${input.text}")
  }
}

fun unescape(input: StringLiteralContext): String {
  val str = input.text.slice(1 until input.text.length - 1)
  val re =
    Regex("""\\(?:(?<char>[^xXuU])|[xX](?<hex>[0-9a-fA-F]{2})|[uU](?<unicode4>[0-9a-fA-F]{4})|[uU]\{(?<unicodeAny>[0-9a-fA-F]{2,})})""")
  return str.replace(re) { match ->
    match.groups["char"]?.value?.let { unescapeChar(it[0]) }
      ?: (match.groups["hex"] ?: match.groups["unicode4"]
      ?: match.groups["unicodeAny"])?.value?.let { unescapeHex(it) }
      ?: throw MxcInternalError(input.ctx, "Unknown escape sequence in $str")
  }
}

private fun unescapeChar(char: Char) = when (char) {
  'b' -> "\b"
  'r' -> "\r"
  'n' -> "\n"
  't' -> "\t"
  'f' -> String(charArrayOf(Char(0x0c)))
  'v' -> String(charArrayOf(Char(0x0b)))
  '0' -> String(charArrayOf(Char(0)))
  else -> String(charArrayOf(char))
}

private fun unescapeHex(hex: String) = String(charArrayOf(Char(hex.toInt(16))))
