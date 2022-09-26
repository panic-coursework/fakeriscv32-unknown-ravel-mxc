package org.altk.lab.mxc.recognizer

import org.altk.lab.mxc.recognizer.MxParser.ProgramContext
import org.antlr.v4.runtime.*
import java.io.InputStream
import kotlin.system.exitProcess

fun parse(inputStream: InputStream): ProgramContext {
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
  val input = CharStreams.fromStream(inputStream)
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
  return root
}
