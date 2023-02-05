package org.altk.lab.mxc.codegen

import java.time.Instant

fun allocateRegisters(func: Function, spillAll: Boolean = false): Function {
  val regs = func.body
    .flatMap { block -> block.body.flatMap { it.defs + it.uses } }
    .toSet()
  val spillsNeeded =
    regs.filter { it is VirtualRegister && it.fallback == null }.toSet()
  if (spillAll || spillsNeeded.size > 10000) {
    println("# too many spills for ${func.name} (${spillsNeeded.size} spills), giving up")
    return fallback(rewriteProgram(func, spillsNeeded))
  }
  val start = Instant.now()
  return AllocContext(func).alloc(start)
}

private fun fallback(func: Function) = func.mapInstruction { inst ->
  (inst.defs + inst.uses)
    .filterIsInstance<VirtualRegister>()
    .fold(inst) { x, reg -> x.replace(reg, reg.fallback!!) }
}

private fun Function.mapInstruction(mapping: (Instruction) -> Instruction) =
  Function(
    name,
    body.map { block ->
      BasicBlock(
        block.label,
        block.body.map(mapping).filter { it !is Mv || it.src != it.dest },
        block.successorNames,
      )
    },
    frameWords,
  )

private fun rewriteProgram(
  func: Function,
  spilledNodes: Set<Register>,
): Function {
  val index = spilledNodes.toList()

  fun offsetOf(reg: Register) =
    ((func.frameWords + index.indexOf(reg)) * wordSize).L

  val body = func.body.map { block ->
    val instructions = block.body.map { inst ->
      val spills = (inst.defs + inst.uses) intersect spilledNodes
      val temporaries = spills
        .mapIndexed { i, reg -> Pair(i, reg) }
        .associateWith { (i, reg) ->
          VirtualRegister("spill:${reg.name}", "t${i + 3}".R)
        }.mapKeys { (key, _) -> key.second }
      val loads = temporaries
        .filterKeys { inst.uses.contains(it) }
        .map { (old, new) -> Load(Load.Width.LW, "sp".R, offsetOf(old), new) }
      val stores = temporaries
        .filterKeys { inst.defs.contains(it) }
        .map { (old, new) ->
          Store(Store.Width.SW, "sp".R, offsetOf(old), new)
        }.reversed()
      val replaced = temporaries.entries
        .fold(inst) { i, (old, new) -> i.replace(old, new) }
      loads + replaced + stores
    }
    BasicBlock(block.label, instructions.flatten(), block.successorNames)
  }

  return Function(func.name, body, func.frameWords + index.size)
}

private fun buildGraph(func: Function): InterferenceGraph {
  val liveOut = dfa(
    func,
    DfaDirection.BACKWARD,
    func.body.associateWith { it.uses },
    func.body.associateWith { it.defs },
  ).out

  val graph = InterferenceGraph(mutableMapOf())
  func.body.forEach { block ->
    val live = liveOut[block]!!.toMutableSet()
    block.body.reversed().forEach { inst ->
      if (inst is Mv) {
        live.removeAll(inst.uses)
      }
      live.addAll(inst.defs)
      live.forEach { reg ->
        inst.defs.forEach { def ->
          if (reg != def) graph.addEdge(reg, def)
        }
      }
      live.removeAll(inst.defs)
      live.addAll(inst.uses)
    }
  }
//  graph.edgesByNode.forEach { (x, y) -> y.forEach { println("${x.name} ${it.name}") } }
  return graph
}

private class InterferenceGraph(val edgesByNode: MutableMap<Register, MutableSet<Register>>) {
  fun neighbors(x: Register): MutableSet<Register> {
    if (edgesByNode[x] == null) edgesByNode[x] = mutableSetOf()
    return edgesByNode[x]!!
  }

  fun hasEdge(x: Register, y: Register) = neighbors(x).contains(y)
  fun degree(x: Register) = neighbors(x).size
  fun addEdge(x: Register, y: Register) {
    neighbors(x).add(y)
    neighbors(y).add(x)
  }
}

private class AllocContext(val func: Function) {
  val graph = buildGraph(func)

  val registers get() = func.body.flatMap { it.defs + it.uses }
  val degree = registers.associateWith { graph.degree(it) }.toMutableMap()
  val weight = mutableMapOf<Register, Int>()
  val refCount = mutableMapOf<Register, Int>()
  val precolored = registers.filterIsInstance<PhysicalRegister>()
  val simplifyWorklist = mutableSetOf<Register>()
  val freezeWorklist = mutableSetOf<Register>()
  val spillWorklist = mutableSetOf<Register>()
  val spilledNodes = mutableSetOf<Register>()
  val coalescedNodes = mutableSetOf<Register>()
  val coloredNodes = mutableSetOf<Register>()
  val selectStack = mutableListOf<Register>()

  val coalescedMoves = mutableSetOf<Mv>()
  val constrainedMoves = mutableSetOf<Mv>()
  val frozenMoves = mutableSetOf<Mv>()
  val worklistMoves =
    func.body.flatMap { it.body.filterIsInstance<Mv>() }.toMutableSet()
  val activeMoves = mutableSetOf<Mv>()

  val moveList = worklistMoves
    .flatMap { mv -> mv.defs.union(mv.uses).map { reg -> Pair(reg, mv) } }
    .groupBy { it.first }
    .mapValues { it.value.map { (_, mv) -> mv }.toMutableSet() }
  val alias = registers.associateWith { it }.toMutableMap()
  val color = registers.associateWith { it as? PhysicalRegister }.toMutableMap()

  @Suppress("PropertyName")
  val K = allocatableRegs.size

  fun alloc(start: Instant?): Function {
//    println(func.text)
//    println("-----------")
    if (start != null && Instant.now().isAfter(start.plusSeconds(30))) {
      println("# regalloc time limit reached for ${func.name}, giving up")
      spilledNodes.addAll(registers.filter { it !is PhysicalRegister })
      return AllocContext(rewriteProgram()).alloc(null)
    }
    makeWorklist()
    calculateWeight()
    val worklists = listOf(
      Pair(simplifyWorklist, ::simplify),
      Pair(worklistMoves, ::coalesce),
      Pair(freezeWorklist, ::freeze),
      Pair(spillWorklist, ::selectSpill),
    )
    while (true) {
      (worklists.find { (wl, _) -> wl.isNotEmpty() } ?: break).second()
    }
    assignColors()
    if (spilledNodes.isNotEmpty()) {
      return AllocContext(rewriteProgram()).alloc(start)
    }
    return colorProgram()
  }

  fun makeWorklist() {
    for (reg in registers) {
      if (reg is PhysicalRegister) continue
      when {
        reg.degree >= K -> spillWorklist
        moveList[reg]?.isNotEmpty() ?: false -> freezeWorklist
        else -> simplifyWorklist
      }.add(reg)
    }
  }

  fun calculateWeight() {
    var instructionId = 0
    val lifetimeStart = mutableMapOf<Register, Int>()
    val lifetimeEnd = mutableMapOf<Register, Int>()
    func.body.forEach { block ->
      block.body.forEach { inst ->
        instructionId++
        (inst.defs + inst.uses).forEach { reg ->
          if (lifetimeStart[reg] == null) lifetimeStart[reg] = instructionId
          lifetimeEnd[reg] = instructionId
          refCount[reg] = refCount.getOrDefault(reg, 0) + 1
        }
      }
    }
    for (reg in registers) {
      weight[reg] = -refCount.getOrDefault(reg, 0) +
        (if (reg.name.startsWith("spill")) -2 else 0) +
        (if (reg.name.startsWith("save")) 2048 else 0) +
        (lifetimeEnd.getOrDefault(reg, 0) - lifetimeStart.getOrDefault(reg, 0))
    }
  }

  fun simplify() {
    val reg = simplifyWorklist.first()
    simplifyWorklist.remove(reg)
    selectStack.add(reg)
    for (x in reg.neighbors) {
      decrementDegree(x)
    }
  }

  fun decrementDegree(reg: Register) {
    degree[reg] = reg.degree - 1
    if (reg.degree != K - 1) return
    for (x in reg.neighbors + reg) {
      for (mv in x.moves) {
        if (activeMoves.remove(mv)) {
          worklistMoves.add(mv)
        }
      }
    }

    (if (reg.isMoveRelated) freezeWorklist else simplifyWorklist).add(reg)
  }

  fun coalesce() {
    val mv = worklistMoves.first()
    worklistMoves.remove(mv)
//    println("considering mv ${mv.dest} ${mv.src}")

    val x = mv.dest.resolve
    val y = mv.src.resolve
    val (u, v) = if (y is PhysicalRegister) Pair(y, x) else Pair(x, y)
//    println("resolved: $u $v")

    fun addWorkList(reg: Register) {
      if (reg is PhysicalRegister || reg.isMoveRelated || reg.degree >= K) return
      freezeWorklist.remove(reg)
      simplifyWorklist.add(reg)
    }

    when {
      u == v -> {
        coalescedMoves.add(mv)
        addWorkList(u)
      }

      v is PhysicalRegister || graph.hasEdge(u, v) -> {
        constrainedMoves.add(mv)
//        println("constrained: $u $v")
        addWorkList(u)
        addWorkList(v)
      }

      if (u is PhysicalRegister) {
        v.neighbors.all {
          it is PhysicalRegister || it.degree < K || graph.hasEdge(it, u)
        }
      } else {
        val uNeighbors = u.neighbors.toSet()
        val conservative = uNeighbors.count { it.degree >= K } +
          v.neighbors.count { it.degree >= K && it !in uNeighbors } < K
        conservative
      } -> {
        coalescedMoves.add(mv)
//        println("combining: $u $v")
        combine(u, v)
        addWorkList(u)
      }

      else -> activeMoves.add(mv)
    }
  }

  fun combine(u: Register, v: Register) {
    listOf(freezeWorklist, spillWorklist).forEach { it.remove(v) }
    coalescedNodes.add(v)
    alias[v] = u
    moveList[u]!!.addAll(moveList[v]!!)
    v.neighbors.forEach {
      if (it != u) {
        graph.addEdge(it, u)
        degree[u] = degree[u]!! + 1
      }
    }
    if (u.degree >= K && freezeWorklist.remove(u)) {
      spillWorklist.add(u)
    }
  }


  fun freeze() {
    val reg = freezeWorklist.first()
    freezeWorklist.remove(reg)
    simplifyWorklist.add(reg)
    freezeMoves(reg)
  }

  fun freezeMoves(u: Register) {
    for (mv in u.moves) {
      val x = mv.dest
      val y = mv.src
      val v = if (y.resolve == u.resolve) x.resolve else y.resolve
      activeMoves.remove(mv)
      frozenMoves.add(mv)
      if (v.moves.isEmpty() && v.degree < K) {
        freezeWorklist.remove(v)
        simplifyWorklist.add(v)
      }
    }
  }


  fun selectSpill() {
    val reg = spillWorklist.maxBy { weight[it]!! }
    spillWorklist.remove(reg)
    simplifyWorklist.add(reg)
    freezeMoves(reg)
  }

  fun assignColors() {
    for (reg in selectStack.reversed()) {
//      println("assigning color for $reg")
//      println("select:")
//      selectStack.forEach { println(it) }
      if (reg is PhysicalRegister) continue
      val okColors = allocatableRegs.toMutableSet()
      for (w in graph.neighbors(reg)) {
//        println("consider $w")
        if (listOf(coloredNodes, precolored).any { it.contains(w.resolve) }) {
//          println("${w.resolve} has color ${color[w.resolve]!!}")
          okColors.remove(color[w.resolve]!!)
        }
      }
      if (okColors.isEmpty()) {
        println("# spilling $reg of weight ${weight[reg]}")
        spilledNodes.add(reg)
      } else {
        coloredNodes.add(reg)
        val moves = moveList[reg]
          ?.map { if (it.src == reg) it.dest else it.src }
          ?.filterIsInstance<PhysicalRegister>()
          ?.toSet()
          ?: setOf()
        color[reg] = moves.find { it in okColors } ?: okColors.first()
      }
    }
    for (reg in coalescedNodes) {
      color[reg] = color[reg.resolve]
    }
  }

  fun rewriteProgram() = rewriteProgram(func, spilledNodes)

  fun colorProgram(): Function {
//    color.filterKeys { x -> x !is PhysicalRegister }.forEach { (old, new) ->
//      println("$old --> $new")
//    }
    val colors = color.filterKeys { x -> x !is PhysicalRegister }.entries
    return func.mapInstruction { inst ->
      colors.fold(inst) { x, (reg, color) -> x.replace(reg, color!!) }
    }
  }

  private val Register.moves
    get(): Set<Mv> {
      val list = moveList[this] ?: return setOf()
      return list
        .filter { activeMoves.contains(it) || worklistMoves.contains(it) }
        .toSet()
    }
  private val Register.isMoveRelated get() = moves.isNotEmpty()
  private val Register.neighbors
    get(): Sequence<Register> {
      val select = selectStack.filter { it !is PhysicalRegister }.toSet()
      return graph.neighbors(this).asSequence().filter {
        !select.contains(it) && !coalescedNodes.contains(it)
      }
    }
  private val Register.degree get() = this@AllocContext.degree[this]!!
  private val Register.resolve
    get(): Register {
      val resolved = alias[this]!!
      if (resolved != this) alias[this] = resolved.resolve
      return alias[this]!!
    }
}
