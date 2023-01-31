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
