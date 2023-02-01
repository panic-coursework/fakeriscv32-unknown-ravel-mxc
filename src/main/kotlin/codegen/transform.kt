package org.altk.lab.mxc.codegen

import org.altk.lab.mxc.codegen.IntI.Type.*
import org.altk.lab.mxc.codegen.IntR.Type.*

abstract class Transformer {
  open fun transform(node: TranslationUnit) = TranslationUnit(
    node.file,
    node.functions.mapNotNull { transform(it) },
    node.data.mapNotNull { transform(it) },
  )

  open fun transform(node: Function): Function? =
    Function(node.name, node.body.mapNotNull { transform(it) }, node.frameWords)

  open fun transform(node: GlobalVariable): GlobalVariable? = node

  open fun transform(node: BasicBlock): BasicBlock? =
    BasicBlock(
      node.label,
      node.body.mapNotNull { transform(it) },
      node.successorNames,
    )

  open fun transform(node: Instruction): Instruction? = node
}

class RemoveRedundantJumps : Transformer() {
  override fun transform(node: Function): Function {
    val nextBlock = node.body.zipWithNext().toMap()
    val body = node.body.map { block ->
      val last = block.body.lastOrNull() ?: return@map block
      val next = nextBlock[block]?.label ?: return@map block
      if ((last as? Jump)?.label?.name == next.name) {
        BasicBlock(block.label, block.body.dropLast(1), block.successorNames)
      } else {
        block
      }
    }
    return Function(node.name, body, node.frameWords)
  }
}

class RemoveNoOps : Transformer() {
  override fun transform(node: Instruction): Instruction? {
    if ((node as? IntI)?.type == IntI.Type.ADDI && (node.imm as? ImmediateLiteral)?.value == 0) {
      if (node.rs1 == node.rd) return null
      return Mv(node.rs1, node.rd)
    }
    if (
      node is IntR &&
      listOf(ADD, SUB, OR, XOR).contains(node.type) &&
      node.rs2 == "zero".R
    ) {
      if (node.rs1 == node.rd) return null
      return Mv(node.rs1, node.rd)
    }
    return node
  }
}

class UseZeroReg : Transformer() {
  override fun transform(node: BasicBlock): BasicBlock {
    val zeroRegs = HashSet<Register>()
    val body = mutableListOf<Instruction>()
    for (inst in node.body) {
      if ((inst as? Li)?.imm?.value == 0) {
        zeroRegs.add(inst.rd)
        body.add(inst)
      } else {
        body.add(zeroRegs.fold(inst) { x, reg -> x.replaceUses(reg, "zero".R) })
        zeroRegs.removeAll(inst.defs)
      }
    }
    return BasicBlock(node.label, body, node.successorNames)
  }
}

class DeduplicateRegs : Transformer() {
  override fun transform(node: BasicBlock): BasicBlock {
    val body = mutableListOf<Instruction>()
    val alias = HashMap<Register, LinkedHashSet<Register>>()
    for (inst in node.body) {
      if (inst is Mv) {
        inst.dest.let { alias[it]?.remove(it) }
        val group = alias[inst.src] ?: linkedSetOf(inst.src)
        if (inst.dest !in group) {
          body.add(Mv(group.first(), inst.dest))
        }
        group.add(inst.dest)
        alias[inst.src] = group
        alias[inst.dest] = group
      } else {
        val uses = inst.uses.associateWith { alias[it]?.first() ?: it }
        body.add(
          uses.asSequence().fold(inst) { i, (x, y) -> i.replaceUses(x, y) })
        inst.defs.forEach {
          alias[it]?.remove(it)
          alias.remove(it)
        }
      }
    }
    return BasicBlock(node.label, body, node.successorNames)
  }
}

class ConvertIntRToIntI : Transformer() {
  private fun rToI(type: IntR.Type) = when (type) {
    ADD -> ADDI
    SLL -> SLLI
    SLT -> SLTI
    SLTU -> SLTIU
    XOR -> XORI
    SRL -> SRLI
    SRA -> SRAI
    OR -> ORI
    AND -> ANDI
    else -> null
  }

  private fun iToR(type: IntI.Type) = when (type) {
    ADDI -> ADD
    SLTI -> SLL
    SLTIU -> SLTU
    XORI -> XOR
    ORI -> OR
    ANDI -> AND
    SLLI -> SLL
    SRLI -> SRL
    SRAI -> SRA
  }

  private fun interpret(type: IntR.Type, lhs: Int, rhs: Int) = when (type) {
    ADD -> lhs + rhs
    SUB -> lhs - rhs
    SLL -> lhs.shl(rhs)
    SLT -> if (lhs < rhs) 1 else 0
    SLTU -> if (lhs.toUInt() < rhs.toUInt()) 1 else 0
    XOR -> lhs.xor(rhs)
    SRL -> lhs.ushr(rhs)
    SRA -> lhs.shr(rhs)
    OR -> lhs.or(rhs)
    AND -> lhs.and(rhs)
    MUL -> lhs * rhs
    MULH, MULHU, MULHSU -> TODO()
    DIV -> if (rhs != 0) lhs / rhs else 0
    DIVU -> if (rhs != 0) (lhs.toUInt() / rhs.toUInt()).toInt() else 0
    REM -> if (rhs != 0) lhs % rhs else 0
    REMU -> if (rhs != 0) (lhs.toUInt() % rhs.toUInt()).toInt() else 0
  }

  override fun transform(node: BasicBlock): BasicBlock {
    val values = HashMap<Register, Int>()
    val body = node.body.map { inst ->
      var const = false
      val replaced = when (inst) {
        is Li -> {
          values[inst.rd] = inst.imm.value
          const = true
          inst
        }

        is IntR -> {
          val lhs = values[inst.rs1]
          val rhs = values[inst.rs2]
          if (lhs == null && rhs == null) {
            inst
          } else if (lhs != null && rhs != null) {
            val res = interpret(inst.type, lhs, rhs)
            values[inst.rd] = res
            const = true
            Li(inst.rd, res.L)
          } else if (rhs != null) {
            val mapped = rToI(inst.type)
            if (inst.type == SUB) {
              IntI(ADDI, inst.rs1, (-rhs).L, inst.rd)
            } else if (mapped != null) {
              IntI(mapped, inst.rs1, rhs.L, inst.rd)
            } else {
              inst
            }
          } else {
            val mapped = rToI(inst.type)
            if (mapped != null && inst.type in listOf(ADD, XOR, OR, AND)) {
              IntI(mapped, inst.rs2, lhs!!.L, inst.rd)
            } else {
              inst
            }
          }
        }

        is IntI -> {
          val value = values[inst.src]
          val imm = (inst.imm as? ImmediateLiteral)?.value
          if (value != null && imm != null) {
            val res = interpret(iToR(inst.type), value, imm)
            values[inst.rd] = res
            const = true
            Li(inst.rd, res.L)
          } else {
            inst
          }
        }

        else -> inst
      }
      if (!const) {
        inst.defs.forEach { values.remove(it) }
      }
      replaced
    }
    return BasicBlock(node.label, body, node.successorNames)
  }
}

class RemoveUnreachableDefinitionsInBlock : Transformer() {
  override fun transform(node: BasicBlock): BasicBlock {
    val body = mutableListOf<Instruction>()
    val defs = mutableListOf<Register>()
    for (inst in node.body.reversed()) {
      when (inst) {
        is La, is Li, is Lui, is Load, is IntI, is IntR ->
          if (inst.defs.any { it !in defs }) body.add(inst)

        else -> body.add(inst)
      }
      defs.addAll(inst.defs)
      defs.removeAll(inst.uses)
      // TODO: make a better approximation
      if (inst is CallLabel) defs.clear()
    }
    return BasicBlock(node.label, body.reversed(), node.successorNames)
  }
}

class RemoveUnreachableDefinitions : Transformer() {
  private val Instruction.def
    get() = if (this is CallLabel) setOf("a0".R) else defs

  override fun transform(node: Function): Function {
    val definitions = node.body
      .flatMap { block ->
        block.body.flatMap { inst -> inst.def.map { Pair(it, inst) } }
      }
      .groupBy { it.first }
      .mapValues { (_, v) -> v.map { it.second } }
    val gen = node.body.associateWith { block ->
      block.body.filter { it.def.isNotEmpty() }.toSet()
    }
    val kill = node.body.associateWith { block ->
      gen[block]!!
        .flatMap { it.def }
        .flatMap { definitions[it]!! }
        .toSet()
        .minus(gen[block]!!)
    }
    val reachingDefinitions = dfa(node, DfaDirection.FORWARD, gen, kill)
    val unused = gen.values.flatten().toMutableSet()
    for (block in node.body) {
      val live = HashSet(reachingDefinitions.in_[block]!!)
      for (inst in block.body) {
        val uses = if (inst is CallLabel) {
          (0..7).map { "a$it".R }
        } else {
          inst.uses
        }
        for (reg in uses) {
          for (def in definitions[reg] ?: listOf()) {
            if (def in live) {
              unused.remove(def)
            }
          }
        }
        val kills = inst.defs.flatMap { definitions[it] ?: listOf() }.toSet()
        live.removeAll(kills)
        live.add(inst)
      }
    }
    val returns = calleeSaveRegs + "ra".R + "a0".R
    for (def in reachingDefinitions.out[node.body.last()]!!) {
      if (def.def.any { it in returns }) {
        unused.remove(def)
      }
    }

    val body = node.body.map { block ->
      BasicBlock(
        block.label,
        block.body.filter { it is CallLabel || it !in unused },
        block.successorNames,
      )
    }

    return Function(node.name, body, node.frameWords)
  }
}

class AllocateRegisters : Transformer() {
  override fun transform(node: Function) = allocateRegisters(node)
}

class NaiveAllocateRegisters : Transformer() {
  override fun transform(node: Function) =
    allocateRegisters(node, spillAll = true)
}
