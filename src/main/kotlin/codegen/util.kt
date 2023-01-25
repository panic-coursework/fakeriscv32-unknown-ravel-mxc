package org.altk.lab.mxc.codegen

private val simpleIdentifier = Regex("""[\-a-zA-Z$._][\-a-zA-Z$._0-9]*""")
fun escape(text: String): String = escape(text.encodeToByteArray())
fun escape(text: ByteArray): String {
  val decoded = text.decodeToString()
  return if (decoded.matches(simpleIdentifier)) {
    decoded
  } else {
    "_${text.joinToString("") { it.toString(16).padStart(2, '0') }}"
  }
}

fun escapeStringLiteral(text: ByteArray): String {
  val bytes = text.map { it.toInt() }.toMutableList()
  if (bytes.last() == 0) bytes.removeLast()
  return bytes.joinToString("") {
    if (it in 0x20..0x7e) {
      it.toChar().toString()
    } else {
      "\\${it.toString(8).padStart(3, '0')}"
    }
  }
}

fun indent(text: String) =
  text.split("\n").joinToString("\n") { if (it != "") "\t$it" else "" }

fun Int.encodeToByteArray() = listOf(
  and(0xff),
  shr(8).and(0xff),
  shr(16).and(0xff),
  shr(24),
).map { it.toByte() }.toByteArray()

fun wordsFromBytes(bytes: ByteArray): List<WordLiteral> {
  val size = (bytes.size + 3) / 4
  return List(size) {
    val base = it * 4
    val word =
      List(4) { i -> bytes.getOrNull(base + i)?.toLong()?.and(0xff) ?: 0 }
        .foldRight(0.toLong()) { b, a -> a.shl(8).or(b) }
    WordLiteral(word)
  }
}
