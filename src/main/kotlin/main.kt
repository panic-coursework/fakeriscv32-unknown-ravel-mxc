package org.altk.lab.mxc

import org.antlr.v4.runtime.*
import java.io.FileInputStream

fun main (args: Array<String>) {
  if (args.isEmpty()) {
    error("Usage: app <command> [file]")
  }
  val inputStream = if (args.size > 1) { FileInputStream(args[1]) } else { System.`in` }
  val input = CharStreams.fromStream(inputStream)
  when (args[0]) {
    "testrig" -> invokeTestRig(input)
    "parse" -> {
      val lexer = MxLexer(input)
      val parser = MxParser(CommonTokenStream(lexer))
      val rules = MxParser.ruleNames.toList()
      parser.hello().toStringTree(rules)
    }
    "ast" -> {
      TODO()
    }
    else -> error("unknown command ${args[0]}")
  }
}
