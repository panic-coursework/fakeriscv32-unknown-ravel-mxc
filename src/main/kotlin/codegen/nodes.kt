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

class Function(val name: ByteArray, val body: List<BasicBlock>) : Node {
  private val prelude
    get() = ".globl ${escape(name)}\n.type ${escape(name)},@function\n"
  override val text
    get() = indent(prelude) + body.joinToString("\n") { it.text }
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

sealed interface Instruction : Node

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


enum class Register : Node {
  ZERO, RA, SP, GP, TP, T0, T1, T2, S0, S1,
  A0, A1, A2, A3, A4, A5, A6, A7,
  S2, S3, S4, S5, S6, S7, S8, S9, S10, S11,
  T3, T4, T5, T6,
  ;

  override val text get() = name.lowercase()
}

val String.R
  get() = when (this) {
    "zero" -> Register.ZERO
    "ra" -> Register.RA
    "sp" -> Register.SP
    "gp" -> Register.GP
    "tp" -> Register.TP
    "t0" -> Register.T0
    "t1" -> Register.T1
    "t2" -> Register.T2
    "fp" -> Register.S0
    "s0" -> Register.S0
    "s1" -> Register.S1
    "a0" -> Register.A0
    "a1" -> Register.A1
    "a2" -> Register.A2
    "a3" -> Register.A3
    "a4" -> Register.A4
    "a5" -> Register.A5
    "a6" -> Register.A6
    "a7" -> Register.A7
    "s2" -> Register.S2
    "s3" -> Register.S3
    "s4" -> Register.S4
    "s5" -> Register.S5
    "s6" -> Register.S6
    "s7" -> Register.S7
    "s8" -> Register.S8
    "s9" -> Register.S9
    "s10" -> Register.S10
    "s11" -> Register.S11
    "t3" -> Register.T3
    "t4" -> Register.T4
    "t5" -> Register.T5
    "t6" -> Register.T6
    else -> throw MxcInternalError(null, "Invalid register $this")
  }

sealed class RTypeInstruction(
  val inst: String,
  val rs1: Register,
  val rs2: Register,
  val rd: Register,
) : Instruction {
  override val text get() = "$inst\t${rd.text}, ${rs1.text}, ${rs2.text}"
}

sealed class ITypeInstruction(
  val inst: String,
  val rs1: Register,
  val imm: Immediate,
  val rd: Register,
) : Instruction {
  override val text get() = "$inst\t${rd.text}, ${rs1.text}, ${imm.text}"
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
}

sealed class BTypeInstruction(
  inst: String,
  rs1: Register,
  rs2: Register,
  imm: Immediate,
) : STypeInstruction(inst, rs1, rs2, imm) {
  override val text get() = "$inst\t${rs1.text}, ${rs2.text}, ${imm.text}"
}

sealed class UTypeInstruction(
  val inst: String,
  val imm: Immediate,
  val rd: Register,
) : Instruction {
  override val text get() = "$inst\t${rd.text}, ${imm.text}"
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
}

class La(val rd: Register, val imm: Label) : PsuedoInstruction {
  override val text get() = "la\t${rd.text}, ${imm.text}"
}

class LoadGlobal(val width: Width, val imm: Label, val rd: Register) :
  PsuedoInstruction {
  enum class Width { LB, LH, LW }

  override val text get() = "${width.name.lowercase()}\t${rd.text}, ${imm.text}"
}

class StoreGlobal(
  val width: Width,
  val base: Register,
  val rt: Register,
  val imm: Label,
) : PsuedoInstruction {
  enum class Width { SB, SH, SW }

  override val text get() = "${width.name.lowercase()}\t${base.text}, ${imm.text}, ${rt.text}"
}

sealed class JumpLabel(val inst: String, val label: Label) : PsuedoInstruction {
  override val text get() = "$inst\t${escape(label.name)}"
}

class Call(label: Label) : JumpLabel("call", label)
class Tail(label: Label) : JumpLabel("tail", label)
class Jump(label: Label) : JumpLabel("j", label)
class JumpReg(val src: Register) : PsuedoInstruction {
  override val text get() = "jr\t${src.text}"
}

object Ret : PsuedoInstruction {
  override val text get() = "ret"
}
