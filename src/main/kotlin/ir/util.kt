package org.altk.lab.mxc.ir

private val simpleIdentifier = Regex("""[\-a-zA-Z$._][\-a-zA-Z$._0-9]*""")
fun escapeIdentifier(id: String) = if (id.matches(simpleIdentifier)) {
  id
} else {
  "\"${id.replace("\\", "\\5c").replace("\"", "\\22")}\""
}

fun indent(text: String) =
  text.split("\n").joinToString("\n") { if (it != "") "  $it" else "" }

class CallGraph(val module: Module) {
  val functions = module.body.filterIsInstance<FunctionDefinition>()
  val functionById = functions.associateBy { it.id }
  val calls = functions.associateWith { func ->
    func.body.flatMap { block -> block.body.filterIsInstance<Call>() }
  }
  val callees = calls.mapValues { (_, insts) ->
    insts.mapNotNull { functionById[it.function.operand] }.toSet()
  }
  val callers = callees
    .flatMap { (caller, callees) -> callees.map { Pair(it, caller) } }
    .groupBy { it.first }
    .mapValues { (_, v) -> v.map { it.second }.toSet() }
  val scc = kosaraju(functions.toSet(), callees, callers)
}

class Scc<T>(val colors: Map<T, Int>, val components: Map<Int, Set<T>>)

private fun <T> kosaraju(
  vertices: Set<T>,
  edges: Map<T, Set<T>>,
  reverse: Map<T, Set<T>>,
): Scc<T> {
  val vis = HashSet<T>()
  val dfn = LinkedHashSet<T>()

  fun dfs1(u: T) {
    vis.add(u)
    for (v in edges[u] ?: setOf()) {
      if (v !in vis) dfs1(v)
    }
    dfn.add(u)
  }

  val colors = HashMap<T, Int>()

  var count = 0
  fun dfs2(u: T) {
    colors[u] = count
    for (v in reverse[u] ?: setOf()) {
      if (v !in colors) dfs2(v)
    }
  }

  for (v in vertices) {
    if (v !in vis) dfs1(v)
  }
  for (v in dfn) {
    if (v !in colors) {
      ++count
      dfs2(v)
    }
  }

  val components = colors
    .map { (x, i) -> Pair(i, x) }
    .groupBy { it.first }
    .mapValues { (_, v) -> v.map { it.second }.toSet() }

  return Scc(colors, components)
}
