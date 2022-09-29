package org.altk.lab.mxc.type

import org.altk.lab.mxc.SourceContext
import org.altk.lab.mxc.ReferenceError
import org.altk.lab.mxc.TypeError

enum class Mutability { MUTABLE, IMMUTABLE }

open class Binding(
  val ctx: SourceContext?,
  val name: String,
  val type: Type,
  val mutability: Mutability,
) {
  var id: Int? = null
}

// dependent types are not supported
class TypeBinding(ctx: SourceContext?, name: String, val content: Type) :
  Binding(ctx, name, MxType, Mutability.IMMUTABLE)

class ReferenceRecord(val env: EnvironmentRecord, val binding: Binding) {
  val isGlobal: Boolean
    get() = env is GlobalEnvironmentRecord
}

open class EnvironmentRecord(val outerEnv: EnvironmentRecord?) {
  private val bindings = HashMap<String, Binding>()

  fun hasBinding(name: String) = name in bindings
  fun getBinding(ctx: SourceContext?, name: String) =
    bindings[name] ?: throw ReferenceError(ctx, "Binding $name not found")

  fun setBinding(binding: Binding) {
    val name = binding.name
    if (!hasBinding(name)) {
      throw ReferenceError(binding.ctx, "Binding $name not found")
    }
    bindings[name] = binding
  }

  fun createBinding(binding: Binding) {
    val name = binding.name
    if (hasBinding(name)) {
      throw ReferenceError(binding.ctx, "Binding $name already declared")
    }
    bindings[name] = binding
  }

  fun getIdentifierReference(
    ctx: SourceContext?,
    name: String,
  ): ReferenceRecord? = if (hasBinding(name)) {
    ReferenceRecord(this, getBinding(ctx, name))
  } else {
    outerEnv?.getIdentifierReference(ctx, name)
  }

  val loopEnv: LoopEnvironmentRecord?
    get() = if (this is LoopEnvironmentRecord) this else outerEnv?.loopEnv
  val functionEnv: FunctionEnvironmentRecord?
    get() = if (this is FunctionEnvironmentRecord) this else outerEnv?.functionEnv
}

class GlobalEnvironmentRecord : EnvironmentRecord(null) {
  fun getType(ctx: SourceContext?, name: String): MxStruct {
    val binding = getBinding(ctx, name) as? TypeBinding
      ?: throw TypeError(ctx, "'$name' does name a type")
    return binding.content as? MxStruct
      ?: throw TypeError(ctx, "Type aliases are not allowed")
  }
}

class LoopEnvironmentRecord(env: EnvironmentRecord) : EnvironmentRecord(env)
class FunctionEnvironmentRecord(env: EnvironmentRecord) : EnvironmentRecord(env)
