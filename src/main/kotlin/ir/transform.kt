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
  open fun transform(node: BasicBlock) = BasicBlock(
    node.label,
    node.body.map { transform(it) },
    node.estimatedFrequency,
  )

  open fun transform(node: Instruction) = node
}

class LocalizeGlobalVariables : Transformer() {
  private lateinit var graph: CallGraph
  private lateinit var uses: Map<Int, Set<String>>
  private lateinit var defs: Map<Int, Set<String>>
  private lateinit var globalVars: Map<String, Value<*>>

  private fun shouldSaveLoad(
    func: FunctionDefinition,
    v: String,
  ): Pair<Boolean, Boolean> {
    val sccId = graph.scc.colors[func]!!
    val used = uses[sccId]?.let { v in it } ?: false
    val defined = defs[sccId]?.let { v in it } ?: false
    return Pair(used, defined)
  }

  private fun shouldLocalize(func: FunctionDefinition, v: String): Boolean {
    var hasLoad = false
    var hasStore = false
    var localizedCost = 0
    var originalCost = 0
    for (block in func.body) {
      for (inst in block.body) {
        val isCost = when (inst) {
          is Load -> {
            val target = inst.target.operand
            val res = target is GlobalIdentifier && target.name == v
            if (res) hasLoad = true
            res
          }

          is Store -> {
            val target = inst.target
            val res = target is GlobalIdentifier && target.name == v
            if (res) hasStore = true
            res
          }

          is GenericCall -> {
            val f = graph.functionById[inst.function.operand]
            if (f != null) {
              val should = shouldSaveLoad(f, v)
              localizedCost +=
                listOf(should.first, should.second).count { it } *
                  block.estimatedFrequency
            }
            false
          }

          else -> false
        }
        if (isCost) originalCost += block.estimatedFrequency
      }
    }
    localizedCost += listOf(hasLoad, hasStore).count { it }
//    System.err.println("${func.id.name} var $v original $originalCost localized $localizedCost")
    return localizedCost < originalCost
  }

  private fun localize(
    func: FunctionDefinition,
    v: String,
  ): FunctionDefinition {
    fun id(name: String) = LocalNamedIdentifier("localized.$v.$name")
    var count = 0
    fun nextId() = id("${count++}")
    fun addrId() = id("addr")
    val value = globalVars[v]!!
    val loads = func.body.flatMap { it.body }
      .filterIsInstance<Load>()
      .filter { (it.target.operand as? GlobalIdentifier)?.name == v }
    val stores = func.body.flatMap { it.body }
      .filterIsInstance<Store>()
      .filter { (it.target as? GlobalIdentifier)?.name == v }
    val entry = func.body.first()

    fun load(): List<Instruction> {
      val load =
        Load(nextId(), Value(GlobalNamedIdentifier(v), PointerType(value.type)))
      return listOf(load, Store(addrId(), load.value))
    }

    fun store(): List<Instruction> {
      val load =
        Load(nextId(), Value(id("addr"), PointerType(value.type)))
      return listOf(load, Store(GlobalNamedIdentifier(v), load.value))
    }
    return FunctionDefinition(
      func.id,
      func.args,
      func.returnType,
      func.body.map { block ->
        val prelude =
          if (block == entry && loads.isNotEmpty()) {
            listOf(Alloca(addrId(), value.type)) + load()
          } else listOf()

        val body = block.body.flatMap { inst ->
          when (inst) {
            is Load -> listOf(
              if (inst in loads) {
                Load(inst.result, Value(addrId(), inst.target.type))
              } else inst
            )

            is Store -> listOf(
              if (inst in stores) Store(addrId(), inst.content) else inst
            )

            is ReturnValue, is ReturnVoid -> {
              store() + inst
            }

            is GenericCall -> {
              val f = graph.functionById[inst.function.operand]
              if (f != null) {
                val (shouldSave, shouldLoad) = shouldSaveLoad(f, v)
                val pre = if (shouldSave) store() else listOf()
                val post = if (shouldLoad) load() else listOf()
                pre + inst + post
              } else listOf(inst)
            }

            else -> listOf(inst)
          }
        }

        BasicBlock(block.label, prelude + body, block.estimatedFrequency)
      },
    )
  }

  override fun transform(node: Module): Module {
    graph = CallGraph(node)
    fun count(what: (Instruction) -> Operand?) =
      graph.scc.components.mapValues { (_, funcs) ->
        funcs.flatMap { func ->
          func.body
            .flatMap { it.body }
            .mapNotNull(what)
            .filterIsInstance<GlobalIdentifier>()
            .map { it.name }
        }.toSet()
      }
    uses = count { (it as? Load)?.target?.operand }
    defs = count { (it as? Store)?.target }
    globalVars = node.body
      .filterIsInstance<GlobalVariableDeclaration>()
      .associate { Pair(it.id.name, Value(it.id, it.value.type)) }
    return super.transform(node)
  }

  override fun transform(node: FunctionDefinition): FunctionDefinition {
    val vars = node.body
      .flatMap { it.body }
      .mapNotNull {
        when (it) {
          is Load -> it.target.operand
          is Store -> it.target
          else -> null
        } as? GlobalIdentifier
      }
      .map { it.name }
      .toSet()
    return vars.fold(node) { x, v ->
      if (shouldLocalize(node, v)) localize(x, v) else x
    }
  }
}

@Suppress("Unused")
class FlattenControlFlow : Transformer() {
  override fun transform(node: FunctionDefinition): FunctionDefinition {
    var nextId = 0
    val name = node.id.name
    fun nextLabel() = Label(LocalNamedIdentifier("$name.flow.L${nextId++}"))

    val cfg = ControlFlow.from(node)
    val edges = cfg.successors
      .flatMap { (pred, list) -> list.map { succ -> Pair(pred, succ) } }
    val intermediaries = HashMap<BasicBlock, MutableList<BasicBlock>>()
    val replacements = HashMap<BasicBlock, HashSet<Pair<Label, Label>>>()
    for ((pred, succ) in edges) {
      val preds = cfg.predecessors[succ] ?: continue
      val succs = cfg.successors[pred] ?: continue
      if (preds.size > 1 && succs.size > 1) {
        val label = nextLabel()
        val intermediary = BasicBlock(
          label,
          listOf(BranchUnconditional(succ.label)),
          pred.estimatedFrequency,
        )
        intermediaries.getOrPut(succ) { mutableListOf() }.add(intermediary)
        replacements.getOrPut(pred) { HashSet() }.add(Pair(succ.label, label))
      }
    }

    val body = node.body.flatMap { block ->
      val repl = replacements[block]
      val newBlock = if (repl == null) {
        block
      } else {
        BasicBlock(
          block.label,
          block.body.map { inst ->
            repl.fold(inst) { i, (x, y) ->
              i.replace(x.operand as LocalIdentifier, y.operand)
            }
          },
          block.estimatedFrequency,
        )
      }
      (intermediaries[block] ?: listOf()) + newBlock
    }

    return FunctionDefinition(node.id, node.args, node.returnType, body)
  }
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
      BasicBlock(
        block.label,
        block.body.filter { it !in removed },
        block.estimatedFrequency,
      )
    }
    return FunctionDefinition(node.id, node.args, node.returnType, body)
  }
}

class LoopInvariantHoisting : Transformer() {
  private lateinit var tree: DominatorTree
  override fun transform(node: FunctionDefinition): FunctionDefinition {
    tree = DominatorTree(node)
    val loops = HashSet<Loop>()
    for (n in node.body) {
      for (h in tree.cfg.successors[n]!!) {
        if (h in tree.ancestors[n]!!) {
          loops.add(naturalLoopOf(n, h))
        }
      }
    }
    val blocksByHeader = loops
      .map { Pair(it.header, it.blocks) }
      .groupBy { it.first }
      .mapValues { (_, v) -> v.flatMap { (_, blocks) -> blocks }.toSet() }

    val invariantOps = loops.associateWith { loop ->
      val ops = loop.blocks
        .flatMap { it.body }
        .filterIsInstance<Operation>()
      val defs = ops.map { it.result }
      val invariants = HashSet<Operation>()
      val invariantLabels = HashSet<LocalIdentifier>()
      while (true) {
        var changed = false
        for (op in ops) {
          if (op is Call || op is Load) continue
          val impureOps = listOf(
            Int32BinaryOperation.Op.SDIV,
            Int32BinaryOperation.Op.SREM,
          )
          if (op is Int32BinaryOperation && op.opType in impureOps) continue
          if (op.uses.all { it !in defs || it in invariantLabels }) {
            if (op !in invariants) {
              changed = true
              invariants.add(op)
              invariantLabels.add(op.result)
            }
          }
        }
        if (!changed) break
      }
      invariants
    }
    val removedOps = invariantOps.values.flatten().toSet()
    val maxLoopByInvariant = invariantOps
      .flatMap { (loop, ops) -> ops.map { Pair(it, loop) } }
      .groupBy { it.first }
      .mapValues { (_, v) -> v.map { it.second }.maxBy { it.blocks.size } }
    val preheaderContents = maxLoopByInvariant
      .map { (op, loop) -> Pair(loop.header, op) }
      .groupBy { it.first }
      .mapValues { (_, v) -> v.map { it.second } }
      .mapValues { (_, ops) ->
        val ids = ops.map { it.result }.toSet()
        val addedIds = mutableSetOf<LocalIdentifier>()
        val list = mutableListOf<Operation>()
        while (list.size < ops.size) {
          for (op in ops) {
            if (op.uses.all { it !in ids || it in addedIds } && op !in list) {
              list.add(op)
              addedIds.add(op.result)
            }
          }
        }
        list
      }
    val preheaders = preheaderContents.mapValues { (header, contents) ->
      val base = (header.label.operand as LocalIdentifier).name
      val label = Label(LocalNamedIdentifier("$base.preheader"))
      val body = contents + BranchUnconditional(header.label)
      BasicBlock(label, body, header.estimatedFrequency)
    }

    val body = node.body.flatMap { block ->
      val insts = block.body.mapNotNull { inst ->
        if (inst in removedOps) {
          null
        } else {
          preheaders
            .filterKeys { block !in blocksByHeader[it]!! }
            .asSequence()
            .map { (hdr, pre) ->
              listOf(hdr, pre).map { it.label.operand as LocalIdentifier }
            }
            .fold(inst) { x, (hdr, pre) -> x.replace(hdr, pre) }
        }
      }
      val newBlock = BasicBlock(block.label, insts, block.estimatedFrequency)

      listOfNotNull(preheaders[block]) + newBlock
    }

    return FunctionDefinition(node.id, node.args, node.returnType, body)
  }

  private class Loop(val header: BasicBlock, val blocks: List<BasicBlock>)

  private fun naturalLoopOf(m: BasicBlock, n: BasicBlock): Loop {
    val worklist = mutableListOf(m)
    val blocks = mutableListOf(m, n)
    while (worklist.isNotEmpty()) {
      val p = worklist.removeLast()
      for (q in tree.cfg.predecessors[p] ?: setOf()) {
        if (q !in blocks) {
          blocks.add(q)
          worklist.add(q)
        }
      }
    }
    return Loop(n, blocks)
  }
}
