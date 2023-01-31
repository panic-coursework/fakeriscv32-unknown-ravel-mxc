package org.altk.lab.mxc.codegen

import org.altk.lab.mxc.MxcInternalError

sealed interface Node {
  val text: String
}

class TranslationUnit(
  val file: String,
  val functions: List<Function>,
  val data: List<GlobalVariable>,
) : Node {
  override val text: String
    get() {
      val prelude = indent(".file \"$file\"\n.text\n")
      val funcs = functions.joinToString("\n\n") { it.text }
      val bss = indent(".section .data\n")
      val datas = data.joinToString("\n") { it.text }
      val epilogue =
        indent("\n.ident \"fakeriscv32-unknown-ravel-mxc version 0.0-dev\"\n")
      return listOf(prelude, funcs, bss, datas, epilogue).joinToString("\n")
    }
}

fun exitLabel(func: String) = Label("$func.exit")

class Function(
  val name: String,
  val body: List<BasicBlock>,
  val frameWords: Int,
) : Node {
  private val signature
    get() = ".globl ${escape(name)}\n.type ${escape(name)},@function\n"

  private val frameSize = alignFrame(frameWords * wordSize)
  // TODO: fp?

  private val prelude
    get() = BasicBlock(
      Label(name),
      if (frameSize == 0) listOf() else {
        listOf(IntI(IntI.Type.ADDI, "sp".R, (-frameSize).L, "sp".R))
      },
      setOf(body.firstOrNull()?.label?.name ?: exitLabel(name).name),
    )
  private val epilogue
    get() = BasicBlock(
      exitLabel(name),
      if (frameSize == 0) listOf(Ret) else {
        listOf(
          IntI(IntI.Type.ADDI, "sp".R, frameSize.L, "sp".R),
          Ret,
        )
      },
      setOf(),
    )
  val blocks = listOf(prelude) + body + epilogue
  override val text
    get() = indent(signature) + blocks.joinToString("\n") { it.text }
}

class GlobalVariable(val label: Label, val body: List<Literal>) : Node {
  override val text: String
    get() {
      val labelText = label.text + ":\n"
      val decl = indent(".globl ${escape(label.name)}\n")
      val data = indent(body.joinToString("\n") { it.text })
      return listOf(labelText, decl, data).joinToString("")
    }
}

sealed interface Literal : Node

class WordLiteral(val value: Long) : Literal {
  override val text get() = ".word $value"
}

class StringLiteral(val value: ByteArray) : Literal {
  override val text get() = ".asciz \"${escapeStringLiteral(value)}\""
}

class BasicBlock(
  val label: Label,
  val body: List<Instruction>,
  val successorNames: Set<String>,
) : Node {
  override val text
    get() = (listOf(label.text + ":") +
      body.map { indent(it.text) }).joinToString("\n")
  private val registers = body
    .fold(Pair(setOf<Register>(), setOf<Register>())) { (defs, uses), bb ->
      Pair(defs.plus(bb.defs), uses.plus(bb.uses.minus(defs)))
    }
  val defs get() = registers.first
  val uses get() = registers.second
}

class Label(val name: String) : Node {
  override val text get() = escape(name)
}

sealed interface Instruction : Node {
  val defs: Set<Register>
  val uses: Set<Register>
  fun replace(x: Register, y: Register): Instruction
  fun replaceUses(x: Register, y: Register): Instruction
  fun replaceDefs(x: Register, y: Register): Instruction
}

private fun Register.r(old: Register, new: Register) =
  if (old == this) new else this

sealed interface Immediate : Node

class ImmediateLiteral(val value: Int) : Immediate {
  override val text get() = "$value"
}

val Int.L get() = ImmediateLiteral(this)

class ImmediateOp(val op: Op, val label: Label) : Immediate {
  enum class Op { HI, LO, PCREL_HI, PCREL_LO }

  override val text get() = "%${op.name.lowercase()}(${escape(label.name)})"
}

class ImmediateLabel(val label: Label) : Immediate {
  override val text get() = escape(label.name)
}


sealed interface Register : Node {
  val name: String
}

enum class PhysicalRegister : Register {
  ZERO, RA, SP, GP, TP, T0, T1, T2, S0, S1,
  A0, A1, A2, A3, A4, A5, A6, A7,
  S2, S3, S4, S5, S6, S7, S8, S9, S10, S11,
  T3, T4, T5, T6,
  ;

  override val text get() = name.lowercase()
}

class VirtualRegister(
  override val name: String,
  val fallback: PhysicalRegister?,
) : Register {
  override val text
    get() = "[vreg@${hashCode().toString(16).padStart(8, '0')} ($name)]"

  override fun equals(other: Any?) = this === other
  override fun hashCode() = super.hashCode()
  override fun toString() = text
}

val String.R
  get() = when (this) {
    "zero" -> PhysicalRegister.ZERO
    "ra" -> PhysicalRegister.RA
    "sp" -> PhysicalRegister.SP
    "gp" -> PhysicalRegister.GP
    "tp" -> PhysicalRegister.TP
    "t0" -> PhysicalRegister.T0
    "t1" -> PhysicalRegister.T1
    "t2" -> PhysicalRegister.T2
    "fp" -> PhysicalRegister.S0
    "s0" -> PhysicalRegister.S0
    "s1" -> PhysicalRegister.S1
    "a0" -> PhysicalRegister.A0
    "a1" -> PhysicalRegister.A1
    "a2" -> PhysicalRegister.A2
    "a3" -> PhysicalRegister.A3
    "a4" -> PhysicalRegister.A4
    "a5" -> PhysicalRegister.A5
    "a6" -> PhysicalRegister.A6
    "a7" -> PhysicalRegister.A7
    "s2" -> PhysicalRegister.S2
    "s3" -> PhysicalRegister.S3
    "s4" -> PhysicalRegister.S4
    "s5" -> PhysicalRegister.S5
    "s6" -> PhysicalRegister.S6
    "s7" -> PhysicalRegister.S7
    "s8" -> PhysicalRegister.S8
    "s9" -> PhysicalRegister.S9
    "s10" -> PhysicalRegister.S10
    "s11" -> PhysicalRegister.S11
    "t3" -> PhysicalRegister.T3
    "t4" -> PhysicalRegister.T4
    "t5" -> PhysicalRegister.T5
    "t6" -> PhysicalRegister.T6
    else -> throw MxcInternalError(null, "Invalid register $this")
  }

sealed class RTypeInstruction(
  val inst: String,
  val rs1: Register,
  val rs2: Register,
  val rd: Register,
) : Instruction {
  override val text get() = "$inst\t${rd.text}, ${rs1.text}, ${rs2.text}"
  override val defs get() = setOf(rd)
  override val uses get() = setOf(rs1, rs2)
}

sealed class ITypeInstruction(
  val inst: String,
  val rs1: Register,
  val imm: Immediate,
  val rd: Register,
) : Instruction {
  override val text get() = "$inst\t${rd.text}, ${rs1.text}, ${imm.text}"
  override val defs get() = setOf(rd)
  override val uses get() = setOf(rs1)
}

sealed class LoadInstruction(
  inst: String,
  rs1: Register,
  imm: Immediate,
  rd: Register,
) : ITypeInstruction(inst, rs1, imm, rd) {
  override val text get() = "$inst\t${rd.text}, ${imm.text}(${rs1.text})"
}

sealed class STypeInstruction(
  val inst: String,
  val rs1: Register,
  val rs2: Register,
  val imm: Immediate,
) : Instruction {
  override val text get() = "$inst\t${rs2.text}, ${imm.text}(${rs1.text})"
  override val defs get() = setOf<Register>()
  override val uses get() = setOf(rs1, rs2)
}

sealed class BTypeInstruction(
  inst: String,
  rs1: Register,
  rs2: Register,
  imm: Immediate,
) : STypeInstruction(inst, rs1, rs2, imm) {
  override val text get() = "$inst\t${rs1.text}, ${rs2.text}, ${imm.text}"
  override val defs get() = setOf<Register>()
  override val uses get() = setOf(rs1, rs2)
}

sealed class UTypeInstruction(
  val inst: String,
  val imm: Immediate,
  val rd: Register,
) : Instruction {
  override val text get() = "$inst\t${rd.text}, ${imm.text}"
  override val defs get() = setOf(rd)
  override val uses get() = setOf<Register>()
}


class Branch(val type: Type, rs1: Register, rs2: Register, dest: Immediate) :
  BTypeInstruction(type.toString().lowercase(), rs1, rs2, dest) {
  enum class Type { BEQ, BNE, BLT, BLTU, BGE, BGEU }

  override fun replace(x: Register, y: Register) =
    Branch(type, rs1.r(x, y), rs2.r(x, y), imm)
  override fun replaceUses(x: Register, y: Register) = replace(x, y)
  override fun replaceDefs(x: Register, y: Register) = this
}

class Lui(imm: Immediate, rd: Register) : UTypeInstruction("lui", imm, rd) {
  override fun replace(x: Register, y: Register) = Lui(imm, rd.r(x, y))
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = replace(x, y)
}

class Auipc(imm: Immediate, rd: Register) : UTypeInstruction("auipc", imm, rd) {
  override fun replace(x: Register, y: Register) = Auipc(imm, rd.r(x, y))
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = replace(x, y)
}

class Jal(imm: Immediate, rd: Register) : UTypeInstruction("jal", imm, rd) {
  override fun replace(x: Register, y: Register) = Jal(imm, rd.r(x, y))
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = replace(x, y)
}

class Jalr(val base: Register, imm: Immediate, rd: Register) :
  ITypeInstruction("jalr", base, imm, rd) {
  override val text get() = "$inst\t${rd.text}, ${imm.text}(${rd.text})"
  override fun replace(x: Register, y: Register) =
    Jalr(base.r(x, y), imm, rd.r(x, y))
  override fun replaceUses(x: Register, y: Register) =
    Jalr(base.r(x, y), imm, rd)
  override fun replaceDefs(x: Register, y: Register) =
    Jalr(base, imm, rd.r(x, y))
}

class Load(val width: Width, val base: Register, imm: Immediate, rd: Register) :
  LoadInstruction(width.name.lowercase(), base, imm, rd) {
  enum class Width { LB, LH, LW, LBU, LHU }

  override fun replace(x: Register, y: Register) =
    Load(width, base.r(x, y), imm, rd.r(x, y))
  override fun replaceUses(x: Register, y: Register) =
    Load(width, base.r(x, y), imm, rd)
  override fun replaceDefs(x: Register, y: Register) =
    Load(width, base, imm, rd.r(x, y))
}

class Store(
  val width: Width,
  val base: Register,
  imm: Immediate,
  val src: Register
) :
  STypeInstruction(width.name.lowercase(), base, src, imm) {
  enum class Width { SB, SH, SW }

  override fun replace(x: Register, y: Register) =
    Store(width, base.r(x, y), imm, src.r(x, y))
  override fun replaceUses(x: Register, y: Register) = replace(x, y)
  override fun replaceDefs(x: Register, y: Register) = this
}

open class IntI(
  val type: Type,
  val src: Register,
  imm: Immediate,
  val dest: Register,
) :
  ITypeInstruction(type.name.lowercase(), src, imm, dest) {
  enum class Type { ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI }

  override fun replace(x: Register, y: Register) =
    IntI(type, src.r(x, y), imm, dest.r(x, y))
  override fun replaceUses(x: Register, y: Register) =
    IntI(type, src.r(x, y), imm, dest)
  override fun replaceDefs(x: Register, y: Register) =
    IntI(type, src, imm, dest.r(x, y))
}

class Mv(src: Register, dest: Register) :
  IntI(Type.ADDI, src, ImmediateLiteral(0), dest) {
  override fun replace(x: Register, y: Register) = Mv(src.r(x, y), dest.r(x, y))
  override fun replaceUses(x: Register, y: Register) = Mv(src.r(x, y), dest)
  override fun replaceDefs(x: Register, y: Register) = Mv(src, dest.r(x, y))
}

class IntR(val type: Type, rs1: Register, rs2: Register, rd: Register) :
  RTypeInstruction(type.name.lowercase(), rs1, rs2, rd) {
  enum class Type {
    ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND,
    MUL, MULH, MULHU, MULHSU, DIV, DIVU, REM, REMU,
  }

  override fun replace(x: Register, y: Register) =
    IntR(type, rs1.r(x, y), rs2.r(x, y), rd.r(x, y))
  override fun replaceUses(x: Register, y: Register) =
    IntR(type, rs1.r(x, y), rs2.r(x, y), rd)
  override fun replaceDefs(x: Register, y: Register) =
    IntR(type, rs1, rs2, rd.r(x, y))
}


sealed interface PsuedoInstruction : Instruction

class Li(val rd: Register, val imm: ImmediateLiteral) : PsuedoInstruction {
  override val text get() = "li\t${rd.text}, ${imm.text}"
  override val defs get() = setOf(rd)
  override val uses get() = setOf<Register>()
  override fun replace(x: Register, y: Register) = Li(rd.r(x, y), imm)
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = replace(x, y)
}

class La(val rd: Register, val imm: Label) : PsuedoInstruction {
  override val text get() = "la\t${rd.text}, ${imm.text}"
  override val defs get() = setOf(rd)
  override val uses get() = setOf<Register>()
  override fun replace(x: Register, y: Register) = La(rd.r(x, y), imm)
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = replace(x, y)
}

class LoadGlobal(val width: Width, val imm: Label, val rd: Register) :
  PsuedoInstruction {
  enum class Width { LB, LH, LW }

  override val text get() = "${width.name.lowercase()}\t${rd.text}, ${imm.text}"
  override val defs get() = setOf(rd)
  override val uses get() = setOf<Register>()
  override fun replace(x: Register, y: Register) =
    LoadGlobal(width, imm, rd.r(x, y))
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = replace(x, y)
}

class StoreGlobal(
  val width: Width,
  val base: Register,
  val rt: Register,
  val imm: Label,
) : PsuedoInstruction {
  enum class Width { SB, SH, SW }

  override val text get() = "${width.name.lowercase()}\t${base.text}, ${imm.text}, ${rt.text}"
  override val defs get() = setOf(rt)
  override val uses get() = setOf<Register>()
  override fun replace(x: Register, y: Register) =
    StoreGlobal(width, base.r(x, y), rt.r(x, y), imm)
  override fun replaceUses(x: Register, y: Register) = replace(x, y)
  override fun replaceDefs(x: Register, y: Register) = this
}

sealed class JumpLabel(val inst: String, val label: Label) : PsuedoInstruction {
  override val text get() = "$inst\t${escape(label.name)}"
  override fun replace(x: Register, y: Register) = this
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = this
}

sealed class CallLabel(inst: String, label: Label) : JumpLabel(inst, label) {
  override val defs get() = callerSaveRegs.toSet()
  override val uses get() = setOf<Register>()
  override fun replace(x: Register, y: Register) = this
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = this
}

class Call(label: Label) : CallLabel("call", label)
class Tail(label: Label) : CallLabel("tail", label)
class Jump(label: Label) : JumpLabel("j", label) {
  override val defs get() = setOf<Register>()
  override val uses get() = setOf<Register>()
}

class JumpReg(val src: Register) : PsuedoInstruction {
  override val text get() = "jr\t${src.text}"
  override val defs get() = setOf<Register>()
  override val uses get() = setOf(src)
  override fun replace(x: Register, y: Register) = JumpReg(src.r(x, y))
  override fun replaceUses(x: Register, y: Register) = replace(x, y)
  override fun replaceDefs(x: Register, y: Register) = this
}

object Ret : PsuedoInstruction {
  override val text get() = "ret"
  override val defs get() = setOf<Register>()
  override val uses get() = setOf<Register>()
  override fun replace(x: Register, y: Register) = this
  override fun replaceUses(x: Register, y: Register) = this
  override fun replaceDefs(x: Register, y: Register) = this
}
