package org.altk.lab.mxc

import org.altk.lab.mxc.ast.Source
import org.altk.lab.mxc.recognizer.MxLexer
import org.altk.lab.mxc.recognizer.MxParser
import org.altk.lab.mxc.recognizer.MxParserBaseVisitor
import org.antlr.v4.gui.TestRig
import org.antlr.v4.runtime.*
import java.io.File
import java.io.FileInputStream
import kotlin.system.exitProcess

private fun run(f: () -> Unit) {
  try {
    f()
  } catch (e: MxcError) {
    e.print()
    if (e !is NotImplemented && e !is MxcInternalError) {
      exitProcess(1)
    }
  }
}

private fun print(f: () -> String) = run { println(f()) }

fun ojMain() {
  run {
    val source = Source("stdin", System.`in`.readAllBytes().decodeToString())
    val module = codegen(source)
    File("output.s").writeText(module.text)
    File("builtin.s").writeBytes(getBuiltin())
  }
}

private fun basename(path: String) = path.split('/').last()

fun main(args: Array<String>) {
  if (args.isEmpty()) {
    error("Usage: app <command> [file]")
  }
  if (args[0] == "oj") return ojMain()

  val inputStream = if (args.size > 1) {
    FileInputStream(args[1])
  } else {
    System.`in`
  }
  val sourceText = inputStream.readAllBytes().decodeToString()
  val filename = if (args.size > 1) basename(args[1]) else "stdin"
  val source = Source(filename, sourceText)

  when (args[0]) {
    "parse" -> print {
      val program = parse(source)
      val rules = MxParser.ruleNames.toList()
      program.tree.toStringTree(rules)
    }

    "ast" -> print { sourceTree(source).toString() }

    "tyck" -> run {
      val rec = typecheck(source, Options.typecheckOnly)
      println("Global bindings:")
      for (binding in rec.globalEnv.bindings) {
        println("${binding.value.name}: ${binding.value.type}")
      }
      println("")
      println(rec.ast.toString(rec))
    }

    "ir"        -> print { ir(source).text }
    "ir-raw"    -> print { ir(source, Options.irRaw).text }
    "ir-no-ssa" -> print { ir(source, Options.irNoSsa).text }

    "codegen"        -> print { codegen(source).text }
    "codegen-no-ssa" -> print { codegen(source, Options.irNoSsa).text }
    "codegen-no-opt" -> print { codegen(source, Options.noOptimizations).text }
    "codegen-no-asm-opt" ->
      print { codegen(source, Options.noAsmOptimizations).text }

    "testrig" -> {
      val input = CharStreams.fromString(sourceText)

      class TestRigClass : TestRig(arrayOf("MxParser", "program", "-gui")) {
        fun invoke(input: CharStream) {
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

      val input = CharStreams.fromString(sourceText)
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
