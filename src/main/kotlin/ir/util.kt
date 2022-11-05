package org.altk.lab.mxc.ir

private val simpleIdentifier = Regex("""[\-a-zA-Z$._][\-a-zA-Z$._0-9]*""")
fun escapeIdentifier(id: String) = if (id.matches(simpleIdentifier)) {
  id
} else {
  "\"${id.replace("\\", "\\5c").replace("\"", "\\22")}\""
}

fun indent(text: String) =
  text.split("\n").joinToString("\n") { if (it != "") "  $it" else "" }
