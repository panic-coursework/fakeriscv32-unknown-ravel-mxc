package org.altk.lab.mxc.codegen

enum class DfaDirection { FORWARD, BACKWARD }

class DfaResult<T>(
  val `in`: Map<BasicBlock, Set<T>>,
  val out: Map<BasicBlock, Set<T>>,
)

// data flow analysis
fun <T> dfa(
  func: Function,
  direction: DfaDirection,
  gen: Map<BasicBlock, Set<T>>,
  kill: Map<BasicBlock, Set<T>>,
): DfaResult<T> {
  val blockByName = func.body.associateBy { it.label.name }
  val successors = func.body.associateWith { block ->
    block.successorNames.mapNotNull { blockByName[it] }.toSet()
  }
  val predecessors = func.body.associateWith { block ->
    func.body.filter { successors[it]!!.contains(block) }.toSet()
  }

  val (prev, next) = when (direction) {
    DfaDirection.FORWARD -> Pair(predecessors, successors)
    DfaDirection.BACKWARD -> Pair(successors, predecessors)
  }

  val out = func.body.associateWith { HashSet<T>() }.toMutableMap()
  val `in` = HashMap<BasicBlock, Set<T>>()
  val worklist = func.body.toMutableList()
  while (worklist.isNotEmpty()) {
    val block = worklist.removeFirst()
    val oldOut = out[block]!!
    val newIn = HashSet<T>()
    for (b in prev[block]!!) {
      newIn.addAll(out[b]!!)
    }
    `in`[block] = newIn
    val newOut = HashSet(newIn)
    newOut.removeAll(kill[block]!!)
    newOut.addAll(gen[block]!!)
    if (oldOut != newOut) {
      out[block] = newOut
      worklist.addAll(next[block]!!)
    }
  }

  return when (direction) {
    DfaDirection.FORWARD -> DfaResult(`in`, out)
    DfaDirection.BACKWARD -> DfaResult(out, `in`)
  }
}
