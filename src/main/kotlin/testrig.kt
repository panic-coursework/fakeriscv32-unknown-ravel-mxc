package org.altk.lab.mxc

import org.antlr.v4.runtime.*
import org.antlr.v4.gui.TestRig

fun invokeTestRig (input: CharStream) {
  class TestRigClass : TestRig(arrayOf("MxParser", "hello", "-gui")) {
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
