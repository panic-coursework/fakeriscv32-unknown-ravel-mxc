package org.altk.lab.mxc

import org.altk.lab.mxc.recognizer.*
import org.antlr.v4.runtime.*
import org.antlr.v4.gui.TestRig
import java.io.InputStream

fun invokeTestRig (inputStream: InputStream) {
  val input = CharStreams.fromStream(inputStream)
  class TestRigClass : TestRig(arrayOf("MxParser", "program", "-gui")) {
    fun invoke (input: CharStream) {
      val lexer = MxLexer(input)
      val tokens = CommonTokenStream(lexer)
      val parser = MxParser(tokens)
      process(lexer, MxParser::class.java, parser, input)
    }
  }
  val testRig = TestRigClass()
  testRig.invoke(input)
}
