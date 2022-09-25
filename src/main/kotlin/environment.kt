package org.altk.lab.mxc

import org.altk.lab.mxc.typechecker.*

class Binding(val ctx: SourceContext?, val name: String, val type: Type) {
  var id: Int? = null
}

class ReferenceRecord(val env: EnvironmentRecord, val binding: Binding)

class EnvironmentRecord(val outerEnv: EnvironmentRecord?, val thisType: Type?) {
  val bindings = HashMap<String, Binding>()

  fun hasBinding(name: String) = name in bindings
  fun getBinding(ctx: SourceContext?, name: String) =
    bindings[name] ?: throw ReferenceError(
      ctx,
      "Binding $name not found"
    )

  fun createBinding(ctx: SourceContext?, name: String, binding: Binding) {
    if (hasBinding(name)) {
      throw ReferenceError(ctx, "Binding $name already declared")
    }
    bindings[name] = binding
  }

  fun getIdentifierReference(
    ctx: SourceContext?,
    name: String
  ): ReferenceRecord? =
    if (hasBinding(name)) {
      ReferenceRecord(this, getBinding(ctx, name))
    } else {
      outerEnv?.getIdentifierReference(ctx, name)
    }
}

interface Scope {
  val env: EnvironmentRecord
}
