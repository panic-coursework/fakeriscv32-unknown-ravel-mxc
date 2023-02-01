package org.altk.lab.mxc.ir

abstract class Transformer {
  open fun transform(node: Module) = Module(node.body.map { transform(it) })
  open fun transform(node: ModuleItem) = when (node) {
    is FunctionDeclaration -> transform(node)
    is FunctionDefinition -> transform(node)
    is GlobalVariableDeclaration -> transform(node)
    is TypeDeclaration -> transform(node)
  }

  open fun transform(node: FunctionDeclaration) = node
  open fun transform(node: FunctionDefinition) = FunctionDefinition(
    node.id,
    node.args,
    node.returnType,
    node.body.map { transform(it) },
  )

  open fun transform(node: GlobalVariableDeclaration) = node
  open fun transform(node: TypeDeclaration) = node
  open fun transform(node: BasicBlock) =
    BasicBlock(node.label, node.body.map { transform(it) })

  open fun transform(node: Instruction) = node
}

class PromoteAllocasToRegisters : Transformer() {
  override fun transform(node: FunctionDefinition) =
    PromoteAllocasToRegistersContext(node).emit()
}

class RemoveUnusedInstructions : Transformer() {
  override fun transform(node: FunctionDefinition): FunctionDefinition {
    val ops = node.body
      .flatMap { it.body }
      .filterIsInstance<Operation>()
      .associateBy { it.result }
    val uses = node.body.flatMap { it.body }.associateWith { it.uses }
    val users = uses
      .flatMap { (node, uses) -> uses.map { Pair(it, node) } }
      .groupBy { (used, _) -> used }
      .mapValues { (_, v) -> v.map { it.second } }
      .toMutableMap()
    val removed = HashSet<Operation>()
    while (true) {
      val unused = users.filterValues { l -> l.all { it in removed } }.keys
      if (unused.isEmpty()) break
      removed.addAll(unused.map { ops[it]!! })
    }
    val body = node.body.map { block ->
      BasicBlock(block.label, block.body.filter { it !in removed })
    }
    return FunctionDefinition(node.id, node.args, node.returnType, body)
  }
}
