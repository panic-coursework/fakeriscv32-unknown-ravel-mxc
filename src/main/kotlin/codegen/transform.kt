package org.altk.lab.mxc.codegen

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
      listOf(
        IntR.Type.ADD,
        IntR.Type.SUB,
        IntR.Type.OR,
        IntR.Type.XOR
      ).contains(node.type) &&
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
      if (inst is Call) defs.clear()
    }
    return BasicBlock(node.label, body.reversed(), node.successorNames)
  }
}

class AllocateRegisters : Transformer() {
  override fun transform(node: Function) = allocateRegisters(node)
}

class NaiveAllocateRegisters : Transformer() {
  override fun transform(node: Function) =
    allocateRegisters(node, spillAll = true)
}
