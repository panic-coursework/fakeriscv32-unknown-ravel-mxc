package org.altk.lab.mxc.ir

import org.altk.lab.mxc.MxcInternalError

class DominatorTree(val func: FunctionDefinition) {
  private val dfNum = HashMap<BasicBlock, Int>()
  private val vertex = HashMap<Int, BasicBlock>()
  private val parent = HashMap<BasicBlock, BasicBlock?>()
  private val bucket = HashMap<BasicBlock, HashSet<BasicBlock>>()
  private val semi = HashMap<BasicBlock, BasicBlock>()
  private val ancestor = HashMap<BasicBlock, BasicBlock>()
  val idom = HashMap<BasicBlock, BasicBlock>()
  private val samedom = HashMap<BasicBlock, BasicBlock>()
  private val best = HashMap<BasicBlock, BasicBlock>()
  private var N = 0

  val blocks = func.body.associateBy { (it.label.operand as Identifier).name }
  val successors = func.body.associateWith { block ->
    block.successors.map { label ->
      blocks[(label.operand as Identifier).name]!!
    }
  }
  val predecessors = successors
    .flatMap { (k, v) -> v.map { Pair(it, k) } }
    .groupBy { it.first }
    .mapValues { (_, v) -> v.map { it.second } }

  private fun dfs(p: BasicBlock?, n: BasicBlock) {
    if (dfNum[n] == null) {
      dfNum[n] = N
      vertex[N] = n
      parent[n] = p
      N++
      for (w in successors[n]!!) {
        dfs(n, w)
      }
    }
  }

  private fun dominators() {
    N = 0
    dfs(null, func.body.first())
    for (i in (N - 1) downTo 1) {
      val n = vertex[i]!!
      val p = parent[n]!!
      var s = p
      for (v in predecessors[n] ?: listOf()) {
        val sp = if (dfNum[v]!! <= dfNum[n]!!) {
          v
        } else {
          semi[ancestorWithLowestSemi(v)]!!
        }
        if (dfNum[sp]!! < dfNum[s]!!) s = sp
      }
      semi[n] = s
      if (bucket[s] == null) bucket[s] = HashSet()
      bucket[s]!!.add(n)
      link(p, n)
      for (v in bucket[p] ?: HashSet()) {
        val y = ancestorWithLowestSemi(v)
        if (semi[y] == semi[v]) {
          idom[v] = p
        } else {
          samedom[v] = y
        }
      }
      bucket.remove(p)
    }
    for (i in 1 until N) {
      val n = vertex[i]!!
      if (samedom[n] != null) {
        idom[n] = idom[samedom[n]!!]!!
      }
    }
  }

  private fun ancestorWithLowestSemi(v: BasicBlock): BasicBlock {
    val a = ancestor[v]!!
    if (ancestor[a] != null) {
      val b = ancestorWithLowestSemi(a)
      ancestor[v] = ancestor[a]!!
      if (dfNum[semi[b]!!]!! < dfNum[semi[best[v]!!]!!]!!) {
        best[v] = b
      }
    }
    return best[v]!!
  }

  private fun link(p: BasicBlock, n: BasicBlock) {
    ancestor[n] = p
    best[n] = n
  }

  init {
    dominators()
  }
}

class PromoteAllocasToRegistersContext(val func: FunctionDefinition) {
  private val allocas = func.body.flatMap { it.body.filterIsInstance<Alloca>() }
  private val vars = allocas.map { it.result }
  private val varType = allocas.associate { Pair(it.result, it.content) }
  private val BasicBlock.uses
    get() = body
      .filterIsInstance<Load>()
      .filter { it.target.operand in vars }
      .map { it.target.operand as LocalIdentifier }
      .toSet()
  private val BasicBlock.defs
    get() = body
      .filterIsInstance<Store>()
      .filter { it.target in vars }
      .map { it.target as LocalIdentifier }
      .toSet()

  private val tree = DominatorTree(func)
  private val children = tree.idom
    .map { (k, v) -> Pair(v, k) }
    .groupBy { it.first }
    .mapValues { (_, v) -> v.map { it.second }.toSet() }
  private val ancestors = func.body.associateWith { HashSet<BasicBlock>() }

  private fun fillAncestors(n: BasicBlock) {
    for (c in children[n] ?: setOf()) {
      ancestors[c]!!.add(n)
      ancestors[c]!!.addAll(ancestors[n]!!)
      fillAncestors(c)
    }
  }

  private val df = HashMap<BasicBlock, Set<BasicBlock>>()
  private fun computeDf(n: BasicBlock) {
    val s = tree.successors[n]!!.filter { tree.idom[it] != n }.toMutableSet()
    for (c in children[n] ?: setOf()) {
      computeDf(c)
      for (w in df[c]!!) {
        if (n !in ancestors[w]!!) {
          s.add(w)
        }
      }
    }
    df[n] = s
  }

  private val phis = func.body.associateWith { HashSet<LocalIdentifier>() }
  private fun placePhis() {
//    tree.idom.forEach { (x, y) ->
//      println("${x.label.operand.text} ${y.label.operand.text}")
//    }
    val defSites = vars.associateWith { HashSet<BasicBlock>() }
    for (n in func.body) {
      for (a in n.defs) {
        defSites[a]!!.add(n)
      }
    }
    for (a in vars) {
      val phiBlocks = func.body.associateWith { HashSet<BasicBlock>() }
      val w = defSites[a]!!
      while (w.isNotEmpty()) {
        val n = w.first()
        w.remove(n)
        for (y in df[n]!!) {
          if (y !in phiBlocks[n]!!) {
            phis[y]!!.add(a)
            phiBlocks[n]!!.add(y)
            w.add(y)
          }
        }
      }
    }
  }

  private val phiMapping = HashMap<BasicBlock, Map<LocalIdentifier, IntArray>>()
  private fun initPhiMappings() {
    for ((block, phiVars) in phis) {
      val size = tree.predecessors[block]?.size ?: 0
      phiMapping[block] = phiVars.associateWith { IntArray(size) }
    }
  }

  private fun varId(a: LocalIdentifier, i: Int) =
    LocalNamedIdentifier("${a.name}\$$i")

  private val count = vars.associateWith { 0 }.toMutableMap()
  private val stack = vars.associateWith { mutableListOf(0) }
  private val phiIndex =
    func.body.associateWith { HashMap<LocalIdentifier, Int>() }
  private val renamedBody = HashMap<BasicBlock, List<Instruction>>()
  private val renamedDefs = HashSet<Pair<LocalIdentifier, Int>>()
  private val replaces = HashMap<LocalIdentifier, Operand>()
  private fun rename(n: BasicBlock) {
//    println("rename ${n.label.text}")
    val indexMap = phiIndex[n]!!
    for (a in phis[n]!!) {
      count[a] = count[a]!! + 1
      val i = count[a]!!
      stack[a]!!.add(i)
      indexMap[a] = i
      renamedDefs.add(Pair(a, i))
    }

    renamedBody[n] = n.body.mapNotNull { s ->
      if (s is Load && s.target.operand in vars) {
        val a = s.target.operand as LocalIdentifier
//        println("load ${a.text} stack is ${stack[a]}")
        val i = stack[a]!!.last()
        replaces[s.result] = varId(a, i)
        null
      } else if (s is Store && s.target in vars) {
        val a = s.target as LocalIdentifier
        count[a] = count[a]!! + 1
        val i = count[a]!!
//        println("store ${a.text} to $i stack ${stack[a]}")
        stack[a]!!.add(i)
        renamedDefs.add(Pair(a, i))
        replaces[varId(a, i)] = s.content.operand
        null
      } else if (s is Alloca) {
        null
      } else s
    }
    for (y in tree.successors[n]!!) {
      val j = tree.predecessors[y]!!.indexOf(n)
      for ((a, phi) in phiMapping[y]!!) {
        phi[j] = stack[a]!!.last()
      }
    }
    for (x in children[n] ?: setOf()) {
      rename(x)
    }
    n.body
      .filterIsInstance<Store>()
      .filter { it.target in vars }
      .forEach { stack[it.target]!!.removeLast() }
    phis[n]!!.forEach { stack[it]!!.removeLast() }
  }

  private fun resolve(x: Operand): Operand {
    if (x !is LocalIdentifier) return x
    val imm = replaces[x]
    if (imm == null || imm == x) return x
    val final = resolve(imm)
    replaces[x] = final
    return final
  }

  private val phiInstructions = HashMap<BasicBlock, List<Phi>>()
  private fun generatePhiInstructions() {
    func.body.forEach { block ->
      val predecessors = tree.predecessors[block] ?: listOf()
      val indexMap = phiIndex[block]!!
//      println(block.label.text)
//      println(phiMapping[block]!!.keys)
      phiInstructions[block] = phiMapping[block]!!.mapNotNull { (a, indices) ->
        val cases = indices.zip(predecessors).mapNotNull { (i, pred) ->
          if (Pair(a, i) in renamedDefs) {
            Phi.Case(valueOf(varId(a, i), varType[a]!!), pred.label)
          } else {
            null
          }
        }
        when (cases.size) {
          0 -> throw MxcInternalError(null, "no cases for phi at var ${a.name}")
          1 -> {
            replaces[varId(a, indexMap[a]!!)] = cases.first().value.operand
            null
          }

          else -> Phi(varId(a, indexMap[a]!!), cases)
        }
      }
    }
  }

  private fun body() = func.body.map { block ->
    val renamed = renamedBody[block]!!
    val body = (phiInstructions[block]!! + renamed).map { inst ->
      replaces.entries.fold(inst) { i, (x, y) -> i.replace(x, resolve(y)) }
    }
    BasicBlock(block.label, body)
  }

  fun emit(): FunctionDefinition {
    val insts = body()
    val args = func.args.map { arg -> valueOf(resolve(arg.operand), arg.type) }
    return FunctionDefinition(func.id, args, func.returnType, insts)
  }

  init {
    val entry = func.body.first()
    fillAncestors(entry)
    computeDf(entry)
    placePhis()
    initPhiMappings()
    rename(entry)
    generatePhiInstructions()
//    replaces.entries.forEach { (x, y) ->
//      println("// ${x.text} -> ${resolve(y).text}")
//    }
  }
}
