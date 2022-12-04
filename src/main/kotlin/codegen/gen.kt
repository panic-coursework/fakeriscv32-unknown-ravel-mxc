package org.altk.lab.mxc.codegen

import org.altk.lab.mxc.ir.*
import org.altk.lab.mxc.ir.BasicBlock as IrBasicBlock
import org.altk.lab.mxc.ir.Call as IrCall
import org.altk.lab.mxc.ir.Instruction as IrInstruction
import org.altk.lab.mxc.ir.Label as IrLabel
import org.altk.lab.mxc.ir.Load as IrLoad
import org.altk.lab.mxc.ir.Store as IrStore
import org.altk.lab.mxc.ir.StringLiteral as IrStringLiteral

class CodegenContext(val file: String, val irModule: Module) {
  fun asm(): TranslationUnit {
    val funcs = irModule.body.filterIsInstance<FunctionDefinition>()
    val vars = irModule.body.filterIsInstance<GlobalVariableDeclaration>()
    return TranslationUnit(file, funcs.map { asm(it) }, vars.map { asm(it) })
  }

  fun saveRegs(regs: List<Pair<Int, String>>) = regs.map {
    Store(Store.Width.SW, "sp".R, it.first.L, it.second.R)
  }

  fun loadRegs(regs: List<Pair<Int, String>>) = regs.map {
    Load(Load.Width.LW, "sp".R, it.first.L, it.second.R)
  }

  private fun asm(ir: FunctionDefinition) = FunctionContext(ir).asm()

  class FunctionContext(private val func: FunctionDefinition) {
    private val virtRegs = ((func.args + func.body.flatMap {
      it.body.filterIsInstance<Operation>().map { op -> op.value }
    }).mapIndexed { i, value ->
      Pair((value.operand as LocalIdentifier).text, i)
    }).toMap()

    private val allocas =
      func.body.flatMap { it.body.filterIsInstance<Alloca>() }
    private val raOffset = 0
    private val fpOffset = raOffset + wordSize
    private val localOffset = fpOffset + wordSize
    private val virtRegOffset = localOffset + allocas.size * wordSize
    private val frameSize = alignFrame(virtRegOffset + virtRegs.size * wordSize)

    private fun offsetOf(id: LocalIdentifier) =
      virtRegs[id.text]!! * wordSize + virtRegOffset

    private fun load(offset: Int, dest: Register) =
      Load(Load.Width.LW, "sp".R, offset.L, dest)

    private fun store(offset: Int, src: Register) =
      Store(Store.Width.SW, "sp".R, offset.L, src)

    private fun loadVirtual(value: Value<*>, dest: Register) =
      when (val op = value.operand) {
        is LocalIdentifier -> load(offsetOf(op), dest)
        is IntegerLiteral -> Li(dest, op.value.L)
        NullLiteral -> Li(dest, 0.L)
        is GlobalIdentifier -> La(
          dest,
          Label((op as GlobalNamedIdentifier).name),
        )

        else -> error("unreachable $op")
      }

    private fun storeVirtual(src: Register, dest: LocalIdentifier) =
      store(offsetOf(dest), src)

    // TODO: indirect calls
    private val Value<*>.label
      get() = Label((operand as GlobalNamedIdentifier).name)

    private val phis = HashMap<String, List<Instruction>>()

    init {
      func.body.forEach { phis[it.label.text] = listOf() }
      func.body.forEach { block ->
        block.body
          .filterIsInstance<Phi>()
          .forEach { phi ->
            phi.cases.forEach { case ->
              val cond = case.condition.text
              phis[cond] = phis[cond]!! + listOf(
                loadVirtual(case.value, "t0".R),
                storeVirtual("t0".R, phi.result),
              )
            }
          }
      }
    }

    fun asm(): Function {
      // TODO: virt2phys
      val body = func.body.map { asm(it) }

      val prelude = listOf(
        Store(Store.Width.SW, "sp".R, (raOffset - frameSize).L, "ra".R),
        Store(Store.Width.SW, "sp".R, (fpOffset - frameSize).L, "fp".R),
        Mv("sp".R, "fp".R),
        IntI(IntI.Type.ADDI, "sp".R, (-frameSize).L, "sp".R),
      ) + func.args.flatMapIndexed { i, arg ->
        if (i <= 7) {
          listOf(storeVirtual("a$i".R, arg.operand as LocalIdentifier))
        } else {
          listOf(
            load((i - 8) * wordSize, "t0".R),
            storeVirtual("t0".R, arg.operand as LocalIdentifier),
          )
        }
      } + allocas.flatMapIndexed { i, alloca ->
        val offset = localOffset + i * wordSize
        listOf(
          IntI(IntI.Type.ADDI, "sp".R, offset.L, "t0".R),
          storeVirtual("t0".R, alloca.result),
        )
      }
      val preludeBlock = BasicBlock(Label(func.id.name), prelude)

      val epilogue = listOf(
        load(raOffset, "ra".R),
        load(fpOffset, "fp".R),
        IntI(IntI.Type.ADDI, "sp".R, frameSize.L, "sp".R),
        Ret,
      )
      val epilogueBlock = BasicBlock(Label("$prefix.exit"), epilogue)

      return Function(
        func.id.name.encodeToByteArray(),
        listOf(preludeBlock) + body + epilogueBlock,
      )
    }

    private val prefix get() = func.id.name

    private fun asm(ir: IrBasicBlock) = BasicBlock(
      asm(ir.label),
      ir.body.flatMap { asm(it, ir.label.text) },
    )

    private val gotoRet get() = Jump(Label("$prefix.exit"))

    private fun asm(ir: IrInstruction, block: String) = when (ir) {
      is CallVoid -> asm(ir)
      is Alloca -> listOf()
      is Icmp -> asm(ir)
      is Int32BinaryOperation -> asm(ir)
      is IntBinaryOperation -> asm(ir)
      is IrCall -> asm(ir)
      is GetElementPtr -> asm(ir)
      is IrLoad -> asm(ir)
      is IrStore -> asm(ir)
      is Phi -> listOf()
      is BranchConditional -> asm(ir, block)
      is BranchUnconditional -> asm(ir, block)
      is ReturnValue -> listOf(loadVirtual(ir.value, "a0".R), gotoRet)
      ReturnVoid -> listOf(gotoRet)
      Unreachable -> listOf()
      is InsertValue, is ExtractValue -> error("unused")
    }

    private fun call(
      args: List<Value<*>>,
      label: Label,
      result: LocalIdentifier?,
    ): List<Instruction> {
      val frameSize =
        if (args.size > 8) alignFrame((args.size - 8) * wordSize) else 0
      val stores = args.flatMapIndexed { i, arg ->
        if (i <= 7) {
          listOf(loadVirtual(arg, "a$i".R))
        } else {
          val offset = (i - 8) * wordSize - frameSize
          listOf(
            loadVirtual(arg, "t0".R),
            Store(Store.Width.SW, "sp".R, offset.L, "t0".R)
          )
        }
      }
      val call = Call(label)
      val storeResult =
        result?.let { listOf(storeVirtual("a0".R, it)) } ?: listOf()
      return if (args.size > 8) {
        stores + listOf(
          IntI(IntI.Type.ADDI, "sp".R, (-frameSize).L, "sp".R),
          call,
          IntI(IntI.Type.ADDI, "sp".R, frameSize.L, "sp".R),
        ) + storeResult
      } else {
        stores + call + storeResult
      }
    }

    private fun asm(ir: CallVoid) = call(ir.args, ir.function.label, null)
    private fun asm(ir: IrCall) = call(ir.args, ir.function.label, ir.result)

    private fun asm(ir: Icmp) = listOf(
      loadVirtual(ir.lhs, "t1".R),
      loadVirtual(ir.rhs, "t2".R),
    ) + when (ir.cond) {
      Icmp.Condition.EQ -> listOf(
        IntR(IntR.Type.XOR, "t1".R, "t2".R, "t0".R),
        IntI(IntI.Type.SLTIU, "t0".R, 1.L, "t0".R),
      )

      Icmp.Condition.NE -> listOf(
        IntR(IntR.Type.XOR, "t1".R, "t2".R, "t0".R),
        IntR(IntR.Type.SLTU, "zero".R, "t0".R, "t0".R),
      )

      Icmp.Condition.SLT -> listOf(IntR(IntR.Type.SLT, "t1".R, "t2".R, "t0".R))
      Icmp.Condition.SGT -> listOf(IntR(IntR.Type.SLT, "t2".R, "t1".R, "t0".R))

      Icmp.Condition.SGE -> listOf(
        IntR(IntR.Type.SLT, "t1".R, "t2".R, "t0".R),
        IntI(IntI.Type.XORI, "t0".R, 1.L, "t0".R),
      )

      Icmp.Condition.SLE -> listOf(
        IntR(IntR.Type.SLT, "t2".R, "t1".R, "t0".R),
        IntI(IntI.Type.XORI, "t0".R, 1.L, "t0".R),
      )
    } + storeVirtual("t0".R, ir.result)

    private fun asm(ir: Int32BinaryOperation) = listOf(
      loadVirtual(ir.lhs, "t1".R),
      loadVirtual(ir.rhs, "t2".R),
      IntR(
        when (ir.opType) {
          Int32BinaryOperation.Op.ADD -> IntR.Type.ADD
          Int32BinaryOperation.Op.SUB -> IntR.Type.SUB
          Int32BinaryOperation.Op.MUL -> IntR.Type.MUL
          Int32BinaryOperation.Op.SDIV -> IntR.Type.DIV
          Int32BinaryOperation.Op.SREM -> IntR.Type.REM
          Int32BinaryOperation.Op.SHL -> IntR.Type.SLL
          Int32BinaryOperation.Op.ASHR -> IntR.Type.SRA
        },
        "t1".R,
        "t2".R,
        "t0".R,
      ),
      storeVirtual("t0".R, ir.result),
    )

    private fun asm(ir: IntBinaryOperation) = listOf(
      loadVirtual(ir.lhs, "t1".R),
      loadVirtual(ir.rhs, "t2".R),
      IntR(
        when (ir.opType) {
          IntBinaryOperation.Op.OR -> IntR.Type.OR
          IntBinaryOperation.Op.AND -> IntR.Type.AND
          IntBinaryOperation.Op.XOR -> IntR.Type.XOR
        },
        "t1".R,
        "t2".R,
        "t0".R,
      ),
      storeVirtual("t0".R, ir.result),
    )

    // TODO: generic GetElementPointer
    private fun asm(ir: GetElementPtr) = listOf(
      loadVirtual(ir.target, "t0".R),
    ) + when (ir.indices.size) {
      2 -> listOf(
        IntI(
          IntI.Type.ADDI,
          "t0".R,
          ((ir.indices[1] as GepIndexLiteral).index * wordSize).L,
          "t0".R,
        ),
      )

      3 -> listOf(
        loadVirtual((ir.indices[2] as GepIndexValue).index, "t1".R),
        IntI(IntI.Type.ADDI, "t1".R, 1.L, "t1".R),
        IntR(IntR.Type.ADD, "t1".R, "t1".R, "t1".R),
        IntR(IntR.Type.ADD, "t1".R, "t1".R, "t1".R),
        IntR(IntR.Type.ADD, "t0".R, "t1".R, "t0".R),
      )

      else -> error("unreachable")
    } + storeVirtual("t0".R, ir.result)

    private fun asm(ir: IrLoad) = listOf(
      loadVirtual(ir.target, "t1".R),
      Load(Load.Width.LW, "t1".R, 0.L, "t0".R),
      storeVirtual("t0".R, ir.result),
    )

    private fun asm(ir: IrStore) = listOf(
      loadVirtual(Value(ir.target, PointerType(null)), "t0".R),
      loadVirtual(ir.content, "t1".R),
      Store(Store.Width.SW, "t0".R, ImmediateLiteral(0), "t1".R),
    )

    private fun asm(ir: BranchConditional, block: String) = phis[block]!! +
      listOf(
        loadVirtual(ir.condition, "t0".R),
        Branch(
          Branch.Type.BNE,
          "t0".R,
          "zero".R,
          ImmediateLabel(asm(ir.consequent)),
        ),
        Jump(asm(ir.alternate)),
      )

    private fun asm(ir: BranchUnconditional, block: String) =
      phis[block]!! + listOf(Jump(asm(ir.dest)))

    private fun asm(ir: IrLabel) =
      Label("$prefix.${(ir.operand as LocalNamedIdentifier).name}")
  }

  private fun asm(ir: GlobalVariableDeclaration) = GlobalVariable(
    Label(ir.id.name),
    wordsFromBytes(asciz(ir.value.operand)),
  )

  private fun asciz(ir: Operand): ByteArray = when (ir) {
    is IntegerLiteral -> ir.value.encodeToByteArray()
    NullLiteral -> ByteArray(4)
    is IrStringLiteral -> ir.content + 0
    Undef, is Identifier -> error("unreachable")
    is AggregateLiteral -> ir.values.map { asciz(it.operand) }
      .fold(byteArrayOf()) { a, b -> a + b }
  }

}
