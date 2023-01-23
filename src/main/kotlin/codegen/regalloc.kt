package org.altk.lab.mxc.codegen

fun allocateRegisters(func: Function) = build(func)

private fun build(func: Function) = simplify(func, buildGraph(func))
private fun buildGraph(func: Function): InterferenceGraph {
  val blockByName = func.body.associateBy { it.label.name }
  val successors = func.body.associateWith { block ->
    block.successorNames.map { blockByName[it]!! }.toSet()
  }
  val liveIn =
    func.body.associateWith { setOf<Register>() }.toMutableMap()
  val liveOut = HashMap<BasicBlock, Set<Register>>()
  var changes = false
  do {
    func.body.forEach { block ->
      liveOut[block] = successors[block]!!
        .map { liveIn[it]!! }
        .fold(setOf()) { a, b -> a.union(b) }
      val newIn = block.uses.union(liveOut[block]!!.minus(block.defs))
      if (newIn != liveIn[block]) {
        liveIn[block] = newIn
        changes = true
      }
    }
  } while (changes)

  val edges = func.body.flatMap { block ->
    block.body
      .foldRight(Pair(liveOut[block]!!, setOf<Pair<Register, Register>>()))
      { inst, (live, edges) ->
        val interferes = live.plus(inst.defs).let {
          if (inst is Mv) it.minus(inst.uses) else it
        }
        Pair(
          live.minus(inst.defs).plus(inst.uses),
          edges.union(interferes.flatMap { reg ->
            inst.defs.flatMap { def -> listOf(Pair(reg, def), Pair(def, reg)) }
          })
        )
      }.second
  }.toSet()
  val edgesByNode = edges
    .groupBy { it.first }
    .map { Pair(it.key, it.value.map { edge -> edge.second }.toSet()) }
    .toMap()
  return InterferenceGraph(edgesByNode)
}

private class InterferenceGraph(val edgesByNode: Map<Register, Set<Register>>) {
  fun edgesOf(x: Register): Set<Register> = edgesByNode[x] ?: setOf()
  fun hasEdge(x: Register, y: Register) = edgesOf(x).contains(y)
  fun degree(x: Register) = edgesOf(x).size
}

enum class RegisterState {
  PRECOLORED, SIMPLIFY, FREEZE, SPILL, SPILLED, COALESCED, COLORED, REMOVED
}

enum class MoveState {
  COALESCABLE, COALESCED, CONSTRAINED, IGNORED, ACTIVE
}

private class AllocState private constructor(
  val func: Function,
  val graph: InterferenceGraph,

  private val moves: Map<>,

  private val registers: Set<Register>,
  private val states: Map<RegisterState, List<Register>>,
  private val mapping: Map<Register, RegisterState>,
) {
  private fun stateOf(x: Register): RegisterState {
    if (x is PhysicalRegister) return RegisterState.PRECOLORED
    TODO()
  }

  companion object {
    fun from(func: Function, graph: InterferenceGraph): AllocState {
      val regs = func.body.flatMap { it.defs + it.uses }.toSet()
      val states = regs.groupBy { initialStateOf(it) }
      val mappings = states
        .flatMap { entry -> entry.value.map { reg -> Pair(reg, entry.key) } }
        .toMap()
      return AllocState(func, graph, regs, states, mappings)
    }

    fun initialStateOf(x: Register): RegisterState {
      TODO()
    }
  }
}

private fun simplify(func: Function, graph: InterferenceGraph): Function {
  TODO()
}

private fun coalesce(func: Function, graph: InterferenceGraph): Function {
  TODO()
}

private fun freeze(func: Function, graph: InterferenceGraph): Function {
  TODO()
}

private fun potentialSpill(func: Function, graph: InterferenceGraph): Function {
  TODO()
}

private fun select(func: Function, graph: InterferenceGraph): Function {
  TODO()
}

private fun actualSpill(func: Function, graph: InterferenceGraph): Function {
  TODO()
}
