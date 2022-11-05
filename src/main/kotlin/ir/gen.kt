package org.altk.lab.mxc.ir

import org.altk.lab.mxc.ast.Expression
import org.altk.lab.mxc.ast.Program
import org.altk.lab.mxc.type.Binding
import org.altk.lab.mxc.type.Mutability
import org.altk.lab.mxc.type.MxFunction
import org.altk.lab.mxc.type.TypecheckRecord
import org.altk.lab.mxc.ast.Node as AstNode
import org.altk.lab.mxc.ast.FunctionDeclaration as AstFunctionDeclaration

class IrGenerationContext(ast: Program) : TypecheckRecord(ast) {
  val fieldIds = HashMap<Binding, Int>()

  val Expression.type get() = types[this]!!
  val Binding.fieldId get() = fieldIds[this]!!
  override val AstNode.env get() = envs[this]!!

  fun ir(): Module {
    val typedefs = classes.map { class_ ->
      val fields = class_.env.bindings
        .filter { it.value.mutability == Mutability.MUTABLE }
        .map { it.value }
        .mapIndexed { i, binding ->
          fieldIds[binding] = i
          binding.type.ir
        }
      TypeDeclaration(
        LocalNamedIdentifier(class_.id.name),
        AggregateType(fields),
      )
    }
    val globalVars = ast.env.bindings
      .filter { it.value.mutability == Mutability.MUTABLE }
      .map { it.value }
      .map { binding ->
        GlobalVariableDeclaration(
          GlobalNamedIdentifier(binding.name),
          binding.type.ir,
        )
      }
    val funcs = functions.map { ir(it) }
    return Module(typedefs + globalVars + funcs)
  }

  fun ir(func: AstFunctionDeclaration): FunctionDefinition {
    val ty = func.env.getBinding(null, func.id.name).type as MxFunction
    val body = mutableListOf<BasicBlock>()
    TODO()
    return FunctionDefinition(
      GlobalNamedIdentifier(func.id.name),
      ty.params.map { it.ir },
      ty.returnType.ir,
      body,
    )
  }
}
