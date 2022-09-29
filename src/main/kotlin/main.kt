package org.altk.lab.mxc

import org.altk.lab.mxc.ast.ast
import org.altk.lab.mxc.recognizer.*
import org.altk.lab.mxc.type.typecheck
import org.antlr.v4.runtime.*
import java.io.FileInputStream
import org.antlr.v4.gui.TestRig
import kotlin.collections.HashSet
import kotlin.system.exitProcess

fun main(args: Array<String>) {
  if (args.isEmpty()) {
    error("Usage: app <command> [file]")
  }
  val inputStream = if (args.size > 1) {
    FileInputStream(args[1])
  } else {
    System.`in`
  }
  when (args[0]) {
    "parse" -> {
      val program = parse(inputStream)
      val rules = MxParser.ruleNames.toList()
      println(program.toStringTree(rules))
    }

    "ast" -> {
      val program = parse(inputStream)
      try {
        val tree = ast(program)
        println(tree)
      } catch (e: MxcError) {
        System.err.println(e)
        exitProcess(1)
      }
    }

    "tyck" -> {
      val program = parse(inputStream)
      try {
        val tree = ast(program)
        typecheck(tree)
      } catch (e: MxcError) {
        System.err.println(e)
        exitProcess(1)
      }
    }

    "testrig" -> {
      val input = CharStreams.fromStream(inputStream)
      class TestRigClass : TestRig(arrayOf("MxParser", "program", "-gui")) {
        fun invoke (input: CharStream) {
          val lexer = MxLexer(input)
          val tokens = CommonTokenStream(lexer)
          val parser = MxParser(tokens)
          process(lexer, MxParser::class.java, parser, input)
        }
      }
      TestRigClass().invoke(input)
    }

    "check-bindings" -> {
      class ExitListener : BaseErrorListener() {
        override fun syntaxError(
          recognizer: Recognizer<*, *>?,
          offendingSymbol: Any?,
          line: Int,
          charPositionInLine: Int,
          msg: String?,
          e: RecognitionException?
        ) {
          print(e)
          exitProcess(0)
        }
      }

      val input = CharStreams.fromStream(inputStream)
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
        override fun visitFunctionDeclaration(ctx: MxParser.FunctionDeclarationContext) {
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
