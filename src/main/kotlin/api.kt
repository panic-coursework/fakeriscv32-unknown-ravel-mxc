package org.altk.lab.mxc

import org.altk.lab.mxc.ast.*
import org.altk.lab.mxc.codegen.*
import org.altk.lab.mxc.ir.*
import org.altk.lab.mxc.type.TypecheckRecord
import org.altk.lab.mxc.ast.Transformer as AstTransformer
import org.altk.lab.mxc.codegen.Transformer as CodegenTransformer
import org.altk.lab.mxc.ir.Transformer as IrTransformer
import org.altk.lab.mxc.recognizer.parse as parse1

data class SourceOptions(
  val passes: List<AstTransformer> = listOf(
    InjectReturnZeroToMain(),
    GenerateEmptyConstructors(),
    DesugarConstructors(),
    DesugarMultiDimensionalNewExpressions(),
    DesugarClassFields(),
    OldLocalizeGlobalVariables(),
    MemoizePureFunctions(),
    MoveGlobalVarsToMain(),
  ),
)

data class IrOptions(
  val ssa: Boolean = true,
  val passes: List<IrTransformer> = listOf(
    LocalizeGlobalVariables(),
    PromoteAllocasToRegisters(),
    RemoveUnusedInstructions(),
    LoopInvariantHoisting(),
  ),
) {
  companion object {
    val noSsa = IrOptions(ssa = false, passes = listOf())
    val raw = IrOptions(passes = listOf())
  }
}

data class CodegenOptions(
  val passes: List<CodegenTransformer> = listOf(
    RemoveNoOps(),
    AllocateRegisters(),
    RemoveRedundantJumps(),
    UseZeroReg(),
    RemoveNoOps(),
    DeduplicateRegs(),
    ConvertIntRToIntI(),
    RemoveNoOps(),
    RemoveUnreachableDefinitions(),
    RemoveUnreachableDefinitionsInBlock(),
  ),
)

data class Options(
  val source: SourceOptions = SourceOptions(),
  val ir: IrOptions = IrOptions(),
  val codegen: CodegenOptions = CodegenOptions(),
) {
  companion object {
    val typecheckOnly = Options(
      source = SourceOptions(passes = listOf(InjectReturnZeroToMain())),
    )
    val irNoSsa = Options(ir = IrOptions.noSsa)
    val irRaw = Options(ir = IrOptions.raw)
    val noOptimizations = Options(
      ir = IrOptions(passes = listOf()),
      codegen = CodegenOptions(passes = listOf(NaiveAllocateRegisters())),
    )
    val noAsmOptimizations = Options(
      codegen = CodegenOptions(passes = listOf(AllocateRegisters())),
    )
  }
}

@Suppress("UNUSED_PARAMETER")
fun parse(source: Source, options: Options = Options()) = parse1(source)

fun sourceTree(source: Source, options: Options = Options()): Program {
  val tree = parse(source, options).ast()
  return options.source.passes.fold(tree) { x, f -> f.transform(x) }
}

fun typecheck(source: Source, options: Options = Options()): TypecheckRecord {
  val tree = sourceTree(source, options)
  return TypecheckRecord(tree)
}

fun ir(source: Source, options: Options = Options()): Module {
  if (!options.ir.ssa && options.ir.passes.isNotEmpty()) {
    throw MxcError(null, "ir passes do not work when ssa disabled")
  }
  val tree = sourceTree(source, options)
  val ir = IrGenerationContext(tree, options.ir.ssa).ir()
  return options.ir.passes.fold(ir) { x, f -> f.transform(x) }
}

fun codegen(source: Source, options: Options = Options()): TranslationUnit {
  val ir = ir(source, options)
  val asm = asm(source.filename ?: "unknown source", ir)
  return options.codegen.passes.fold(asm) { x, f -> f.transform(x) }
}

private object Dummy

fun getBuiltin() =
  Dummy.javaClass.classLoader.getResource("builtins.s")!!.readBytes()
