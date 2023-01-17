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

class Function(
  val name: String,
  val body: List<BasicBlock>,
  val frameWords: Int,
) : Node {
  private val signature
    get() = ".globl ${escape(name)}\n.type ${escape(name)},@function\n"

  private val raOffset = frameWords * wordSize
  private val fpOffset = raOffset + wordSize
  private val frameSize = alignFrame(fpOffset + wordSize)

  private val prelude
    get() = BasicBlock(
      Label(name),
      listOf(
        Store(Store.Width.SW, "sp".R, (raOffset - frameSize).L, "ra".R),
        Store(Store.Width.SW, "sp".R, (fpOffset - frameSize).L, "fp".R),
        Mv("sp".R, "fp".R),
        IntI(IntI.Type.ADDI, "sp".R, (-frameSize).L, "sp".R),
      ),
    )
  private val epilogue
    get() = BasicBlock(
      Label("$name.exit"),
      listOf(
        Load(Load.Width.LW, "sp".R, raOffset.L, "ra".R),
        Load(Load.Width.LW, "sp".R, fpOffset.L, "fp".R),
        IntI(IntI.Type.ADDI, "sp".R, frameSize.L, "sp".R),
        Ret,
      ),
    )
  private val instructions = listOf(prelude) + body + epilogue
  override val text
    get() = indent(signature) + instructions.joinToString("\n") { it.text }
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

class WordLiteral(val value: Int) : Literal {
  override val text get() = ".word $value"
}

class StringLiteral(val value: ByteArray) : Literal {
  override val text get() = ".asciz \"${escapeStringLiteral(value)}\""
}

class BasicBlock(val label: Label, val body: List<Instruction>) : Node {
  override val text
    get() = (listOf(label.text + ":") +
      body.map { indent(it.text) }).joinToString("\n")
}

class Label(val name: ByteArray) : Node {
  constructor(name: String) : this(name.encodeToByteArray())

  override val text get() = escape(name)
}

sealed interface Instruction : Node {
  val defines: List<Register>
  val uses: List<Register>
}

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


sealed interface Register : Node

enum class PhysicalRegister : Register {
  ZERO, RA, SP, GP, TP, T0, T1, T2, S0, S1,
  A0, A1, A2, A3, A4, A5, A6, A7,
  S2, S3, S4, S5, S6, S7, S8, S9, S10, S11,
  T3, T4, T5, T6,
  ;

  override val text get() = name.lowercase()
}

class VirtualRegister : Register {
  override val text get() = "[${this}]"
  override fun equals(other: Any?) = this === other
  override fun hashCode() = javaClass.hashCode()
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
  override val defines get() = listOf(rd)
  override val uses get() = listOf(rs1, rs2)
}

sealed class ITypeInstruction(
  val inst: String,
  val rs1: Register,
  val imm: Immediate,
  val rd: Register,
) : Instruction {
  override val text get() = "$inst\t${rd.text}, ${rs1.text}, ${imm.text}"
  override val defines get() = listOf(rd)
  override val uses get() = listOf(rs1)
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
  override val defines get() = listOf<Register>()
  override val uses get() = listOf(rs1, rs2)
}

sealed class BTypeInstruction(
  inst: String,
  rs1: Register,
  rs2: Register,
  imm: Immediate,
) : STypeInstruction(inst, rs1, rs2, imm) {
  override val text get() = "$inst\t${rs1.text}, ${rs2.text}, ${imm.text}"
  override val defines get() = listOf<Register>()
  override val uses get() = listOf(rs1, rs2)
}

sealed class UTypeInstruction(
  val inst: String,
  val imm: Immediate,
  val rd: Register,
) : Instruction {
  override val text get() = "$inst\t${rd.text}, ${imm.text}"
  override val defines get() = listOf(rd)
  override val uses get() = listOf<Register>()
}


class Branch(val type: Type, rs1: Register, rs2: Register, dest: Immediate) :
  BTypeInstruction(type.toString().lowercase(), rs1, rs2, dest) {
  enum class Type { BEQ, BNE, BLT, BLTU, BGE, BGEU }
}

class Lui(imm: Immediate, rd: Register) : UTypeInstruction("lui", imm, rd)
class Auipc(imm: Immediate, rd: Register) : UTypeInstruction("auipc", imm, rd)
class Jal(imm: Immediate, rd: Register) : UTypeInstruction("jal", imm, rd)

class Jalr(base: Register, imm: Immediate, rd: Register) :
  ITypeInstruction("jalr", base, imm, rd) {
  override val text get() = "$inst\t${rd.text}, ${imm.text}(${rd.text})"
}

class Load(val width: Width, base: Register, imm: Immediate, rd: Register) :
  LoadInstruction(width.name.lowercase(), base, imm, rd) {
  enum class Width { LB, LH, LW, LBU, LHU }
}

class Store(val width: Width, base: Register, imm: Immediate, src: Register) :
  STypeInstruction(width.name.lowercase(), base, src, imm) {
  enum class Width { SB, SH, SW }
}

open class IntI(val type: Type, src: Register, imm: Immediate, dest: Register) :
  ITypeInstruction(type.name.lowercase(), src, imm, dest) {
  enum class Type { ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI }
}

class Mv(src: Register, dest: Register) :
  IntI(Type.ADDI, src, ImmediateLiteral(0), dest)

class IntR(val type: Type, rs1: Register, rs2: Register, rd: Register) :
  RTypeInstruction(type.name.lowercase(), rs1, rs2, rd) {
  enum class Type {
    ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND,
    MUL, MULH, MULHU, MULHSU, DIV, DIVU, REM, REMU,
  }
}


sealed interface PsuedoInstruction : Instruction

class Li(val rd: Register, val imm: ImmediateLiteral) : PsuedoInstruction {
  override val text get() = "li\t${rd.text}, ${imm.text}"
  override val defines get() = listOf(rd)
  override val uses get() = listOf<Register>()
}

class La(val rd: Register, val imm: Label) : PsuedoInstruction {
  override val text get() = "la\t${rd.text}, ${imm.text}"
  override val defines get() = listOf(rd)
  override val uses get() = listOf<Register>()
}

class LoadGlobal(val width: Width, val imm: Label, val rd: Register) :
  PsuedoInstruction {
  enum class Width { LB, LH, LW }

  override val text get() = "${width.name.lowercase()}\t${rd.text}, ${imm.text}"
  override val defines get() = listOf(rd)
  override val uses get() = listOf<Register>()
}

class StoreGlobal(
  val width: Width,
  val base: Register,
  val rt: Register,
  val imm: Label,
) : PsuedoInstruction {
  enum class Width { SB, SH, SW }

  override val text get() = "${width.name.lowercase()}\t${base.text}, ${imm.text}, ${rt.text}"
  override val defines get() = listOf(rt)
  override val uses get() = listOf<Register>()
}

sealed class JumpLabel(val inst: String, val label: Label) : PsuedoInstruction {
  override val text get() = "$inst\t${escape(label.name)}"
}

sealed class CallLabel(inst: String, label: Label) : JumpLabel(inst, label) {
  override val defines get() = callerSaveRegs
  override val uses get() = listOf<Register>()
}

class Call(label: Label) : CallLabel("call", label)
class Tail(label: Label) : CallLabel("tail", label)
class Jump(label: Label) : JumpLabel("j", label) {
  override val defines get() = listOf<Register>()
  override val uses get() = listOf<Register>()
}
class JumpReg(val src: Register) : PsuedoInstruction {
  override val text get() = "jr\t${src.text}"
  override val defines get() = listOf<Register>()
  override val uses get() = listOf(src)
}

object Ret : PsuedoInstruction {
  override val text get() = "ret"
  override val defines get() = listOf<Register>()
  override val uses get() = listOf<Register>()
}
