package org.altk.lab.mxc

import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import java.io.FileInputStream
import java.util.*
import kotlin.collections.HashSet
import kotlin.system.exitProcess

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
      println(parser.program().toStringTree(rules))
    }
    "ast" -> {
      TODO()
    }
    "check-bindings" -> {
      class ExitListener : ANTLRErrorListener {
        override fun syntaxError (recognizer: Recognizer<*, *>?, offendingSymbol: Any?, line: Int, charPositionInLine: Int, msg: String?, e: RecognitionException?) {
          print(e)
          exitProcess(0)
        }
        override fun reportAmbiguity (recognizer: Parser?, dfa: DFA?, startIndex: Int, stopIndex: Int, exact: Boolean, ambigAlts: BitSet?, configs: ATNConfigSet?) {
          exitProcess(0)
        }
        override fun reportAttemptingFullContext (recognizer: Parser?, dfa: DFA?, startIndex: Int, stopIndex: Int, conflictingAlts: BitSet?, configs: ATNConfigSet?) {
          exitProcess(0)
        }
        override fun reportContextSensitivity (recognizer: Parser?, dfa: DFA?, startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet?) {
          exitProcess(0)
        }
      }
      val lexer = MxLexer(input)
      lexer.removeErrorListeners()
      lexer.addErrorListener(ExitListener())
      val parser = MxParser(CommonTokenStream(lexer))
      parser.removeErrorListeners()
      parser.addErrorListener(ExitListener())
      val prog = parser.program()
      class Visitor : MxParserBaseVisitor<Unit>() {
        var funcs = HashSet<String>()
        var vars = HashSet<String>()
        override fun visitFunctionDeclaration (ctx: MxParser.FunctionDeclarationContext) {
          val name = ctx.identifier().IdentifierName().text
          funcs.add(name)
          return super.visitFunctionDeclaration(ctx)
        }

        override fun visitLexicalBinding(ctx: MxParser.LexicalBindingContext) {
          val name = ctx.identifier().IdentifierName().text
          vars.add(name)
          return super.visitLexicalBinding(ctx)
        }
      }
      val v = Visitor()
      prog.accept(v)
      val intersection = v.funcs.intersect(v.vars)
      if (intersection.isNotEmpty()) {
        println("Ambiguity found:")
        for (name in intersection) {
          print(name)
          print(' ')
        }
        println("")
        exitProcess(1)
      }
    }
    else -> error("unknown command ${args[0]}")
  }
}
