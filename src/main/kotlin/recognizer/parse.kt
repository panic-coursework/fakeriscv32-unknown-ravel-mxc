package org.altk.lab.mxc.recognizer

import org.altk.lab.mxc.ast.Source
import org.altk.lab.mxc.recognizer.MxParser.ProgramContext
import org.antlr.v4.runtime.*
import kotlin.system.exitProcess

class ParseRecord(val source: Source, val tree: ProgramContext)

fun parse(source: Source): ParseRecord {
  var hasErrors = false

  class ExitListener : BaseErrorListener() {
    override fun syntaxError(
      recognizer: Recognizer<*, *>?,
      offendingSymbol: Any?,
      line: Int,
      charPositionInLine: Int,
      msg: String?,
      e: RecognitionException?,
    ) {
      System.err.println("In input:$line:$charPositionInLine (at $offendingSymbol): $msg")
      hasErrors = true
    }
  }

  val listener = ExitListener()
  val input = CharStreams.fromString(source.sourceText)
  val lexer = MxLexer(input)
  lexer.removeErrorListeners()
  lexer.addErrorListener(listener)
  val parser = MxParser(CommonTokenStream(lexer))
  parser.removeErrorListeners()
  parser.addErrorListener(listener)
  val root = parser.program()
  if (hasErrors) {
    exitProcess(1)
  }
  return ParseRecord(source, root)
}
