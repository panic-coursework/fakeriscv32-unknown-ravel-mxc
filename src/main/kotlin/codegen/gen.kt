package org.altk.lab.mxc.codegen

import org.altk.lab.mxc.ir.*
import org.altk.lab.mxc.ir.BasicBlock as IrBasicBlock
import org.altk.lab.mxc.ir.Call as IrCall
import org.altk.lab.mxc.ir.Instruction as IrInstruction
import org.altk.lab.mxc.ir.Label as IrLabel
import org.altk.lab.mxc.ir.Load as IrLoad
import org.altk.lab.mxc.ir.Store as IrStore
import org.altk.lab.mxc.ir.StringLiteral as IrStringLiteral

fun asm(file: String, irModule: Module): TranslationUnit {
  val funcs = irModule.body.filterIsInstance<FunctionDefinition>()
  val vars = irModule.body.filterIsInstance<GlobalVariableDeclaration>()
  return TranslationUnit(file, funcs.map { asm(it) }, vars.map { asm(it) })
}

private fun asm(ir: FunctionDefinition) =
  allocateRegisters(FunctionCodegenContext(ir).asm())

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

private class FunctionCodegenContext(private val func: FunctionDefinition) {
  private val virtRegs = ((func.args + func.body.flatMap {
    it.body.filterIsInstance<Operation>().map { op -> op.value }
  }).map { value ->
    val id = value.operand as LocalIdentifier
    Pair(id, VirtualRegister("var.${id.name}", null))
  }).toMap()

  private val LocalIdentifier.R get() = virtRegs[this]!!

  private val allocas =
    func.body.flatMap { it.body.filterIsInstance<Alloca>() }

  private fun loadVirtual(value: Value<*>, dest: Register) =
    when (val op = value.operand) {
      is LocalIdentifier -> Mv(op.R, dest)
      is IntegerLiteral -> Li(dest, op.value.L)
      NullLiteral -> Li(dest, 0.L)
      is GlobalIdentifier -> La(
        dest,
        Label((op as GlobalNamedIdentifier).name),
      )

      else -> error("unreachable $op")
    }

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
            phis[cond] = phis[cond]!! + loadVirtual(case.value, phi.result.R)
          }
        }
    }
  }

  fun asm(): Function {
    val body = func.body.map { asm(it) }

    val savedRegs = (calleeSaveRegs + "ra".R)
      .associateWith { VirtualRegister("save.$it", null) }

    val epilogue = savedRegs.map { (phy, virt) -> Mv(virt, phy) }
    val epilogueBlock = BasicBlock(
      Label("$prefix.epilogue"),
      epilogue,
      setOf(exitLabel(prefix).name),
    )

    val saveRegs = savedRegs.map { (phy, virt) -> Mv(phy, virt) }
    val saveArgs = func.args.flatMapIndexed { i, arg ->
      if (i <= 7) {
        listOf(Mv("a$i".R, (arg.operand as LocalIdentifier).R))
      } else {
        val dest = (arg.operand as LocalIdentifier).R
        listOf(Load(Load.Width.LW, "sp".R, ((i - 8) * wordSize).L, dest))
      }
    }
    val initAllocas = allocas.flatMapIndexed { i, alloca ->
      val offset = i * wordSize
      listOf(IntI(IntI.Type.ADDI, "sp".R, offset.L, alloca.result.R))
    }
    val prelude = saveRegs + saveArgs + initAllocas
    val preludeBlock = BasicBlock(
      Label("$prefix.prelude"),
      prelude,
      setOf(body.firstOrNull()?.label?.name ?: epilogueBlock.label.name),
    )

    return Function(
      func.id.name,
      listOf(preludeBlock) + body + epilogueBlock,
      allocas.size,
    )
  }

  private val prefix get() = func.id.name

  private fun asm(ir: IrBasicBlock) = BasicBlock(
    asm(ir.label),
    ir.body.flatMap { asm(it, ir.label.text) },
    if (ir.successors.isEmpty()) {
      setOf("$prefix.epilogue")
    } else {
      ir.successors.map { "$prefix.${(it.operand as NamedIdentifier).name}" }
        .toSet()
    },
  )

  private val gotoRet get() = Jump(Label("$prefix.epilogue"))

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
        val temp = VirtualRegister("a$i", null)
        listOf(
          loadVirtual(arg, temp),
          Store(Store.Width.SW, "sp".R, offset.L, temp)
        )
      }
    }
    val call = Call(label)
    val storeResult =
      result?.let { listOf(Mv("a0".R, it.R)) } ?: listOf()
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

  private fun asm(ir: Icmp): List<Instruction> {
    val lhs = VirtualRegister("icmp.lhs", "t0".R)
    val rhs = VirtualRegister("icmp.rhs", "t1".R)
    val dest = ir.result.R
    return listOf(
      loadVirtual(ir.lhs, lhs),
      loadVirtual(ir.rhs, rhs),
    ) + when (ir.cond) {
      Icmp.Condition.EQ -> listOf(
        IntR(IntR.Type.XOR, lhs, rhs, dest),
        IntI(IntI.Type.SLTIU, dest, 1.L, dest),
      )

      Icmp.Condition.NE -> listOf(
        IntR(IntR.Type.XOR, lhs, rhs, dest),
        IntR(IntR.Type.SLTU, "zero".R, dest, dest),
      )

      Icmp.Condition.SLT -> listOf(IntR(IntR.Type.SLT, lhs, rhs, dest))
      Icmp.Condition.SGT -> listOf(IntR(IntR.Type.SLT, rhs, lhs, dest))

      Icmp.Condition.SGE -> listOf(
        IntR(IntR.Type.SLT, lhs, rhs, dest),
        IntI(IntI.Type.XORI, dest, 1.L, dest),
      )

      Icmp.Condition.SLE -> listOf(
        IntR(IntR.Type.SLT, rhs, lhs, dest),
        IntI(IntI.Type.XORI, dest, 1.L, dest),
      )
    }
  }

  private fun asmBinaryOperation(
    lhs: Value<*>,
    rhs: Value<*>,
    op: IntR.Type,
    result: LocalIdentifier,
  ): List<Instruction> {
    val lhsReg = VirtualRegister("$op.lhs", "t0".R)
    val rhsReg = VirtualRegister("$op.rhs", "t1".R)
    return listOf(
      loadVirtual(lhs, lhsReg),
      loadVirtual(rhs, rhsReg),
      IntR(op, lhsReg, rhsReg, result.R),
    )
  }

  private fun asm(ir: Int32BinaryOperation) = asmBinaryOperation(
    ir.lhs, ir.rhs,
    when (ir.opType) {
      Int32BinaryOperation.Op.ADD -> IntR.Type.ADD
      Int32BinaryOperation.Op.SUB -> IntR.Type.SUB
      Int32BinaryOperation.Op.MUL -> IntR.Type.MUL
      Int32BinaryOperation.Op.SDIV -> IntR.Type.DIV
      Int32BinaryOperation.Op.SREM -> IntR.Type.REM
      Int32BinaryOperation.Op.SHL -> IntR.Type.SLL
      Int32BinaryOperation.Op.ASHR -> IntR.Type.SRA
    },
    ir.result,
  )

  private fun asm(ir: IntBinaryOperation) = asmBinaryOperation(
    ir.lhs, ir.rhs,
    when (ir.opType) {
      IntBinaryOperation.Op.OR -> IntR.Type.OR
      IntBinaryOperation.Op.AND -> IntR.Type.AND
      IntBinaryOperation.Op.XOR -> IntR.Type.XOR
    },
    ir.result,
  )

  // TODO: generic GetElementPointer
  private fun asm(ir: GetElementPtr): List<Instruction> {
    val address = ir.result.R
    val offset = VirtualRegister("gep.offset", "t0".R)
    return listOf(
      loadVirtual(ir.target, address),
    ) + when (ir.indices.size) {
      2 -> listOf(
        IntI(
          IntI.Type.ADDI,
          address,
          ((ir.indices[1] as GepIndexLiteral).index * wordSize).L,
          address,
        ),
      )

      3 -> listOf(
        loadVirtual((ir.indices[2] as GepIndexValue).index, offset),
        IntI(IntI.Type.ADDI, offset, 1.L, offset),
        IntI(IntI.Type.SLLI, offset, 2.L, offset),
        IntR(IntR.Type.ADD, address, offset, address),
      )

      else -> error("unreachable")
    }
  }

  private fun asm(ir: IrLoad): List<Instruction> {
    val address = VirtualRegister("load.addr", "t0".R)
    return listOf(
      loadVirtual(ir.target, address),
      Load(Load.Width.LW, address, 0.L, ir.result.R),
    )
  }

  private fun asm(ir: IrStore): List<Instruction> {
    val address = VirtualRegister("store.addr", "t0".R)
    val value = VirtualRegister("store.value", "t1".R)
    return listOf(
      loadVirtual(Value(ir.target, PointerType(null)), address),
      loadVirtual(ir.content, value),
      Store(Store.Width.SW, address, ImmediateLiteral(0), value),
    )
  }

  private fun asm(ir: BranchConditional, block: String): List<Instruction> {
    val cond = VirtualRegister("br.cond", "t0".R)
    return phis[block]!! +
      listOf(
        loadVirtual(ir.condition, cond),
        Branch(
          Branch.Type.BNE,
          cond,
          "zero".R,
          ImmediateLabel(asm(ir.consequent)),
        ),
        Jump(asm(ir.alternate)),
      )
  }

  private fun asm(ir: BranchUnconditional, block: String) =
    phis[block]!! + Jump(asm(ir.dest))

  private fun asm(ir: IrLabel) =
    Label("$prefix.${(ir.operand as NamedIdentifier).name}")
}
