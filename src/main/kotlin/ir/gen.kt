package org.altk.lab.mxc.ir

import org.altk.lab.mxc.MxcInternalError
import org.altk.lab.mxc.NotImplemented
import org.altk.lab.mxc.ast.*
import org.altk.lab.mxc.ast.Identifier
import org.altk.lab.mxc.type.*
import org.altk.lab.mxc.ast.Transformer as AstTransformer
import org.altk.lab.mxc.ast.FunctionDeclaration as AstFunctionDeclaration
import org.altk.lab.mxc.ast.Identifier as AstIdentifier
import org.altk.lab.mxc.ast.IntegerLiteral as AstIntegerLiteral
import org.altk.lab.mxc.ast.Node as AstNode
import org.altk.lab.mxc.ast.NullLiteral as AstNullLiteral
import org.altk.lab.mxc.ast.StringLiteral as AstStringLiteral
import org.altk.lab.mxc.type.Type as AstType

private const val LOOP_WEIGHT = 10

class IrGenerationContext(ast: Program, val ssa: Boolean = true) :
  TypecheckRecord(ast) {
  private val AstType.ir: Type get() = ir(this)
  fun ir(type: AstType) = when (type) {
    MxInt -> Int32Type
    MxNullptr -> PointerType(null)
    MxString -> MxStringType
    is MxArray -> MxArrayType(type.content.ir)
    MxBool -> BoolType
    MxVoid -> VoidType
    is MxStruct -> typeFromStruct[type.name]!!
    is MxFunction ->
      MxClosureType(type.params.map { it.ir }, type.returnType.ir)

    MxBot, MxTop, MxHole, MxType ->
      throw MxcInternalError(null, "Unexpected type $this")
  }

  val valueFromReference = HashMap<ReferenceRecord, Value<*>>()
  private val typeFromStruct = HashMap<String, MxStructType>()

  private inner class FunctionContext {
    val nextIndex = HashMap<String, Int>()
    val blocks = mutableListOf<BasicBlockContext>()
    fun nextId(name: String): LocalNamedIdentifier {
      val ix = nextIndex[name] ?: 0
      nextIndex[name] = ix + 1
      return LocalNamedIdentifier(if (ix > 0) "$name.$ix" else name)
    }

    fun createBasicBlock(name: String, freq: Int): BasicBlockContext {
      val ctx = BasicBlockContext(this, Label(nextId(name)), ssa, freq)
      blocks.add(ctx)
      return ctx
    }

    fun createBasicBlockWithName(name: String, freq: Int): BasicBlockContext {
      val ctx =
        BasicBlockContext(this, Label(LocalNamedIdentifier(name)), ssa, freq)
      blocks.add(ctx)
      return ctx
    }

    fun emit(): List<BasicBlock> {
      val list = blocks.map { it.emit() }.filter { it.body.isNotEmpty() }
      val blockFromLabel = list.associateBy { it.label }
      val reachable = mutableSetOf<BasicBlock>()
      fun visit(block: BasicBlock) {
        if (reachable.contains(block)) return
        reachable.add(block)
        val term = block.body[block.body.size - 1] as TerminatorInstruction
        term.successors.forEach { visit(blockFromLabel[it]!!) }
      }
      visit(list[0])
      return reachable.toList()
    }

    fun debug() {
      blocks
        .map { it.emit() }
        .filter { it.body.isNotEmpty() }
        .forEach { System.err.println(it.text) }
    }
  }

  private inner class BasicBlockContext(
    val func: FunctionContext,
    val label: Label,
    val ssa: Boolean,
    val freq: Int,
  ) {
    val body = mutableListOf<Instruction>()
    private val AstType.ir: Type get() = this@IrGenerationContext.ir(this)

    fun registerEnv(env: EnvironmentRecord) {
      env.bindings.values
        .sortedBy { it.name }
        .map { ReferenceRecord(env, it) }
        .forEach { registerReference(it) }
    }

    private fun registerReference(ref: ReferenceRecord) {
      if (!valueFromReference.containsKey(ref)) {
        val id = func.nextId(ref.binding.name)
        val ty = ref.binding.type.ir
        val value = valueOf(id, if (ssa) PointerType(ty) else ty)
        valueFromReference[ref] = value
        if (ssa) {
          body.add(Alloca(id, ty))
        } else {
          body.add(Move(valueOf(IntegerLiteral(0), Int32Type), id))
        }
      }
    }

    fun emit() = BasicBlock(label, body.toList(), freq)
  }

  private class LoopContext(
    val breakTarget: BasicBlockContext,
    val continueTarget: BasicBlockContext,
  )

  val Expression.type get() = types[this]!!
  val ReferenceRecord.value get() = valueFromReference[this]!!
  val AstIdentifier.value get() = references[this]!!.value
  override val AstNode.env get() = envs[this]!!

  val strings = HashMap<AstStringLiteral, GlobalNamedIdentifier>()

  private fun collectStrings(): List<ModuleItem> {
    val decls = mutableListOf<ModuleItem>()

    class CollectStringsTransform : AstTransformer() {
      var nextId = 0
      override fun transform(node: AstStringLiteral): AstStringLiteral {
        val id = GlobalNamedIdentifier(".str.$nextId")
        strings[node] = id
        ++nextId
        val bytes = node.value.encodeToByteArray()
        decls.add(
          GlobalVariableDeclaration(
            id,
            Value(
              AggregateLiteral(
                listOf(
                  Value(IntegerLiteral(bytes.size), Int32Type),
                  StringLiteral(bytes).value,
                ),
              ),
              AggregateType(
                listOf(Int32Type, ArrayType(CharType, bytes.size + 1)),
              ),
            ),
          ),
        )
        return node
      }
    }
    CollectStringsTransform().transform(ast)
    return decls
  }

  fun ir(): Module {
    val strings = collectStrings()

    classes.forEach {
      val id = LocalNamedIdentifier("struct.${it.id.name}")
      typeFromStruct[it.id.name] =
        MxStructType(id, mutableListOf(), mutableMapOf())
    }
    val typedefs = classes.map { clazz ->
      val fields = clazz.env.bindings
        .filter { it.value.mutability == Mutability.MUTABLE }
        .map { Pair(it.key, it.value.type.ir) }
      val ty = typeFromStruct[clazz.id.name]!!
      ty.indexFromName as MutableMap +=
        fields.mapIndexed { i, p -> Pair(p.first, i) }.toMap()
      (ty.pointee!! as AggregateType).subtypes as MutableList +=
        fields.map { it.second }
      TypeDeclaration(
        LocalNamedIdentifier(clazz.id.name),
        AggregateType(fields.map { it.second }),
      )
    }
    val globalVars = ast.env.bindings
      .filter { it.value.mutability == Mutability.MUTABLE }
      .map { it.value }
      .map { binding ->
        val ty = binding.type.ir
        val id = GlobalNamedIdentifier(binding.name)
        valueFromReference[ReferenceRecord(globalEnv, binding)] =
          valueOf(id, PointerType(ty))
        GlobalVariableDeclaration(id, Value(NullLiteral, ty))
      }
    val funcs = functions.map { ir(it) }
    val methods = classes.flatMap { clazz ->
      clazz.body.filterIsInstance<AstFunctionDeclaration>()
        .map { func -> ir(func, clazz) }
    }
    return Module(prelude + strings + typedefs + globalVars + funcs + methods)
  }

  private fun ir(
    func: AstFunctionDeclaration,
    `class`: ClassDeclaration? = null,
  ): FunctionDefinition {
    val ty =
      func.env.outerEnv!!.getBinding(null, func.id.name).type as MxFunction
    val ctx = FunctionContext()
    val thisArg = `class`?.let {
      valueOf(
        LocalNamedIdentifier("this"),
        (`class`.env as ClassEnvironmentRecord).type.ir,
      )
    }
    val thisArgs = thisArg?.let { listOf(it) } ?: listOf()
    val params = ty.params
      .zip(func.params)
      .map { (type, param) -> valueOf(ctx.nextId(param.id.name), type.ir) }
    val block = ctx.createBasicBlockWithName("entry", 1)
    if (ssa) {
      block.registerEnv(func.env)
      block.body += func.params.mapIndexed { i, param ->
        Store(param.id.value.operand, params[i])
      }
    } else {
      func.params.zip(params).forEach { (ast, value) ->
        valueFromReference[references[ast.id]!!] = value
      }
    }
    try {
      val exit = ir(block, func.body, null)
      if (ty.returnType is MxVoid) {
        exit.body.add(ReturnVoid)
      }
    } catch (e: Exception) {
      System.err.println("Current function:")
      ctx.debug()
      throw e
    }
    val classPrefix = `class`?.id?.name?.let { "$it." } ?: ""
    return FunctionDefinition(
      GlobalNamedIdentifier(classPrefix + func.id.name),
      thisArgs + params,
      ty.returnType.ir,
      ctx.emit(),
    )
  }

  private fun ir(
    entry: BasicBlockContext,
    node: Statement,
    loop: LoopContext?,
  ): BasicBlockContext = when (node) {
    is BlockStatement -> {
      entry.registerEnv(node.env)
      node.body.fold(entry) { block, stmt -> ir(block, stmt, loop) }
    }

    is BreakStatement -> {
      entry.body.add(BranchUnconditional(loop!!.breakTarget.label))
      entry.func.createBasicBlock("break.after", entry.freq)
    }

    is ContinueStatement -> {
      entry.body.add(BranchUnconditional(loop!!.continueTarget.label))
      entry.func.createBasicBlock("continue.after", entry.freq)
    }

    is EmptyStatement -> entry
    is ExpressionStatement -> ir(entry, node.expression).exit

    is IfStatement -> {
      val test = ir(entry, node.test)
      val testRes = test.result!!.asType<BoolType>()
      val consequentBlock =
        entry.func.createBasicBlock("if.consequent", entry.freq)
      val exitBlock = entry.func.createBasicBlock("if.exit", entry.freq)
      consequentBlock.registerEnv(node.consequent.env)
      val consequentExit = ir(consequentBlock, node.consequent, loop)
      consequentExit.body.add(BranchUnconditional(exitBlock.label))

      if (node.alternate != null) {
        val alternateBlock =
          entry.func.createBasicBlock("if.alternate", entry.freq)
        alternateBlock.registerEnv(node.alternate.env)
        val br = BranchConditional(
          testRes,
          consequentBlock.label,
          alternateBlock.label,
        )
        test.exit.body.add(br)
        val alternateExit = ir(alternateBlock, node.alternate, loop)
        alternateExit.body.add(BranchUnconditional(exitBlock.label))
      } else {
        val br =
          BranchConditional(testRes, consequentBlock.label, exitBlock.label)
        test.exit.body.add(br)
      }

      exitBlock
    }

    is ForStatement -> {
      entry.registerEnv(node.env)

      val initExit = when (node.init) {
        null -> entry
        is VariableDeclaration -> ir(entry, node.init, loop)
        is Expression -> ir(entry, node.init).exit
      }
      val freq = entry.freq * LOOP_WEIGHT
      val testBlock = entry.func.createBasicBlock("for.test", freq)
      initExit.body.add(BranchUnconditional(testBlock.label))
      val bodyBlock = entry.func.createBasicBlock("for.body", freq)
      val updateBlock = entry.func.createBasicBlock("for.update", freq)
      val updateExit =
        node.update?.let { ir(updateBlock, it).exit } ?: updateBlock
      updateExit.body.add(BranchUnconditional(testBlock.label))
      val exitBlock = entry.func.createBasicBlock("for.exit", entry.freq)

      if (node.test == null) {
        testBlock.body.add(BranchUnconditional(bodyBlock.label))
      } else {
        val test = ir(testBlock, node.test)
        val br = BranchConditional(
          test.result!!.asType(),
          bodyBlock.label,
          exitBlock.label,
        )
        test.exit.body.add(br)
      }

      val loopSelf =
        LoopContext(breakTarget = exitBlock, continueTarget = updateBlock)
      ir(bodyBlock, node.body, loopSelf).body
        .add(BranchUnconditional(updateBlock.label))

      exitBlock
    }

    is WhileStatement -> {
      val freq = entry.freq * LOOP_WEIGHT
      val testBlock = entry.func.createBasicBlock("while.test", freq)
      entry.body.add(BranchUnconditional(testBlock.label))
      val bodyBlock = entry.func.createBasicBlock("while.body", freq)
      val exitBlock = entry.func.createBasicBlock("while.exit", entry.freq)
      bodyBlock.registerEnv(node.body.env)

      val test = ir(testBlock, node.test)
      val testRes = test.result!!.asType<BoolType>()
      val br = BranchConditional(testRes, bodyBlock.label, exitBlock.label)
      test.exit.body.add(br)
      val endBr = BranchUnconditional(testBlock.label)

      val loopSelf =
        LoopContext(breakTarget = exitBlock, continueTarget = testBlock)
      ir(bodyBlock, node.body, loopSelf).body.add(endBr)

      exitBlock
    }

    is ReturnStatement -> {
      if (node.argument == null) {
        entry.body.add(ReturnVoid)
      } else {
        val res = ir(entry, node.argument)
        res.exit.body.add(ReturnValue(res.result!!))
      }
      entry.func.createBasicBlock("return.after", entry.freq)
    }

    is VariableDeclaration ->
      node.declarations.fold(entry) { block, decl ->
        decl.init?.let {
          val res = ir(block, it)
          res.exit.body.add(if (ssa) {
            Store(decl.id.value.operand, res.result!!)
          } else {
            Move(res.result!!, decl.id.value.operand as LocalIdentifier)
          })
          res.exit
        } ?: entry
      }
  }

  private class ExpressionResult(
    val exit: BasicBlockContext,
    val result: Value<*>?,
  )

  private fun resFromOp(
    exit: BasicBlockContext,
    op: Operation,
  ): ExpressionResult {
    exit.body.add(op)
    return ExpressionResult(exit, op.value)
  }

  private val Expression.isPure
    get(): Boolean = when (this) {
      is AssignmentExpression, is CallExpression, is UpdateExpression, is NewExpression -> false
      is LambdaExpression, is Identifier, is Literal -> true
      is ComputedMemberExpression -> `object`.isPure && prop.isPure
      is MemberExpression -> `object`.isPure
      is GroupExpression -> expression.isPure
      is UnaryExpression -> argument.isPure
      is BinaryExpression ->
        left.isPure && right.isPure && operator != BinaryOperator.DIV && operator != BinaryOperator.REM
    }

  private fun ir(entry: BasicBlockContext, node: Expression): ExpressionResult =
    when (node) {
      is AssignmentExpression -> {
        val lhs = getLhsPointer(entry, node.left)
        val rhs = ir(lhs.exit, node.right)
        val lhsPointer = lhs.result!!.operand
        if (!ssa && node.left is AstIdentifier && lhsPointer is LocalIdentifier) {
          rhs.exit.body.add(Move(rhs.result!!, lhsPointer))
        } else {
          rhs.exit.body.add(Store(lhsPointer, rhs.result!!))
        }
        ExpressionResult(rhs.exit, null)
      }

      is BinaryExpression ->
        if (!node.isPure && (node.operator == BinaryOperator.AND || node.operator == BinaryOperator.OR)) {
          val default = node.operator == BinaryOperator.OR
          val opname = node.operator.toString()
          val left = ir(entry, node.left)
          val nextBlock =
            entry.func.createBasicBlock("$opname.next", entry.freq)
          val exitBlock =
            entry.func.createBasicBlock("$opname.exit", entry.freq)
          val br = BranchConditional(
            left.result!!.asType(),
            if (default) exitBlock.label else nextBlock.label,
            if (default) nextBlock.label else exitBlock.label,
          )
          left.exit.body.add(br)
          val nextRight = ir(nextBlock, node.right)
          val exitBr = BranchUnconditional(exitBlock.label)
          nextRight.exit.body.add(exitBr)
          val id = entry.func.nextId(opname)
          val defaultV = if (default) 1 else 0
          val phi = Phi(
            id,
            listOf(
              Phi.Case(nextRight.result!!, nextBlock.label),
              Phi.Case(
                Value(IntegerLiteral(defaultV), BoolType),
                left.exit.label
              ),
            ),
          )
          resFromOp(exitBlock, phi)
        } else {
          val left = ir(entry, node.left)
          val right = ir(left.exit, node.right)
          val id = entry.func.nextId(node.operator.toString())
          val op = when (node.left.type) {
            MxString -> when (node.operator) {
              BinaryOperator.ADD -> Call(
                id,
                Value(
                  GlobalNamedIdentifier("string.+"),
                  FunctionType(
                    listOf(MxStringType, MxStringType),
                    MxStringType,
                  ),
                ),
                listOf(left.result!!, right.result!!),
              )

              BinaryOperator.LE, BinaryOperator.GE, BinaryOperator.LT,
              BinaryOperator.GT, BinaryOperator.EQ, BinaryOperator.NE -> {
                val cmp = Call(
                  entry.func.nextId("strcmp"),
                  Value(
                    GlobalNamedIdentifier("string.<=>"),
                    FunctionType(
                      listOf(MxStringType, MxStringType),
                      Int32Type,
                    ),
                  ),
                  listOf(left.result!!, right.result!!),
                )
                right.exit.body.add(cmp)
                Icmp(
                  id,
                  node.operator.irCompare,
                  Int32Type,
                  cmp.value.asType(),
                  Value(IntegerLiteral(0), Int32Type),
                )
              }

              else -> throw MxcInternalError(null, "Invalid string op")
            }

            is MxStruct, is MxArray, MxBool -> when (node.operator) {
              BinaryOperator.AND, BinaryOperator.OR -> IntBinaryOperation(
                id,
                BoolType,
                if (node.operator == BinaryOperator.AND) IntBinaryOperation.Op.AND else IntBinaryOperation.Op.OR,
                left.result!!.asType(),
                right.result!!.asType(),
              )

              else -> Icmp(
                id,
                node.operator.irCompare,
                node.left.type.ir as ComparableType,
                left.result!!.asType(),
                right.result!!.asType(),
              )
            }

            MxInt -> when (node.operator) {
              BinaryOperator.ADD, BinaryOperator.SUB,
              BinaryOperator.MUL, BinaryOperator.DIV, BinaryOperator.REM,
              BinaryOperator.SHL, BinaryOperator.SHR -> Int32BinaryOperation(
                id,
                node.operator.ir32,
                left.result!!.asType(),
                right.result!!.asType(),
              )

              BinaryOperator.BIT_AND, BinaryOperator.BIT_OR, BinaryOperator.BIT_XOR -> IntBinaryOperation(
                id,
                Int32Type,
                node.operator.irBitwise,
                left.result!!.asType(),
                right.result!!.asType(),
              )

              BinaryOperator.LE, BinaryOperator.GE, BinaryOperator.LT, BinaryOperator.GT,
              BinaryOperator.EQ, BinaryOperator.NE -> Icmp(
                id,
                node.operator.irCompare,
                Int32Type,
                left.result!!.asType(),
                right.result!!.asType(),
              )

              BinaryOperator.AND, BinaryOperator.OR -> error("unreachable")
            }

            else -> throw MxcInternalError(null, "Type error on binop")
          }
          resFromOp(right.exit, op)
        }

      is CallExpression -> {
        val (block0, funcId, thisArg) = when (node.callee) {
          is MemberExpression -> {
            val obj = ir(entry, node.callee.`object`)
            val className =
              when (val type = node.callee.`object`.type as ObjectType) {
                is MxArray -> "array"
                MxString -> "string"
                is MxStruct -> type.name
              }
            val methodName = node.callee.prop.name
            val func = GlobalNamedIdentifier("$className.$methodName")
            Triple(obj.exit, func, obj.result)
          }

          is AstIdentifier ->
            Triple(entry, GlobalNamedIdentifier(node.callee.name), null)

          else -> throw NotImplemented(node.ctx, "lambda: ${node.callee}")
        }
        val (block1, args0) =
          node.arguments.fold(Pair(block0, listOf<Value<*>>())) { pair, arg ->
            val res = ir(pair.first, arg)
            Pair(res.exit, pair.second + res.result!!)
          }
        val thisArgs = thisArg?.let { listOf(it) } ?: listOf()
        val args = thisArgs + args0
        val params = args.map { it.type }
        val func = Value(funcId, FunctionType(params, node.type.ir))
        if (node.type.ir is VoidType) {
          val call = CallVoid(func, args)
          block1.body.add(call)
          ExpressionResult(block1, null)
        } else {
          val op = Call(block1.func.nextId("call.${funcId.name}"), func, args)
          resFromOp(block1, op)
        }
      }

      is LambdaExpression -> throw NotImplemented(node.ctx, "lambda")
      is ComputedMemberExpression -> {
        val ptr = getLhsPointer(entry, node)
        val op = Load(entry.func.nextId("computed.load"), ptr.result!!.asType())
        resFromOp(ptr.exit, op)
      }

      is GroupExpression -> ir(entry, node.expression)
      is AstIdentifier -> {
        // TODO: generate closure if it's a function
        val ptr = getLhsPointer(entry, node)
        if (!ssa && ptr.result!!.operand is LocalIdentifier) {
          ptr
        } else {
          val res = Load(entry.func.nextId(node.name), ptr.result!!.asType())
          resFromOp(ptr.exit, res)
        }
      }

      is MemberExpression -> {
        val ptr = getLhsPointer(entry, node)
        val op = Load(entry.func.nextId("member.load"), ptr.result!!.asType())
        resFromOp(ptr.exit, op)
      }

      is UpdateExpression -> {
        val target = getLhsPointer(entry, node.argument)
        val mem =
          ssa || node.argument !is AstIdentifier || target.result!!.operand !is LocalIdentifier
        val load = if (mem) {
          Load(entry.func.nextId("update.value"), target.result!!.asType())
        } else {
          Move(target.result!!, entry.func.nextId("update.value"))
        }
        val res = Int32BinaryOperation(
          entry.func.nextId("update.result"),
          when (node.operator) {
            UpdateOperator.INC -> Int32BinaryOperation.Op.ADD
            UpdateOperator.DEC -> Int32BinaryOperation.Op.SUB
          },
          load.value.asType(),
          Value(IntegerLiteral(1), Int32Type),
        )
        val store = if (mem) {
          Store(target.result.operand, res.value)
        } else {
          Move(res.value, target.result.operand as LocalIdentifier)
        }
        entry.body += listOf(load, res, store)
        ExpressionResult(
          entry, when (node) {
            is PostfixUpdateExpression -> load
            is PrefixUpdateExpression -> res
          }.value
        )
      }

      is BooleanLiteral -> ExpressionResult(
        entry,
        Value(
          IntegerLiteral(
            when (node.value) {
              true -> 1
              false -> 0
            }
          ),
          BoolType,
        ),
      )

      is AstIntegerLiteral ->
        ExpressionResult(entry, Value(IntegerLiteral(node.value), Int32Type))

      is AstNullLiteral ->
        ExpressionResult(entry, Value(NullLiteral, node.type.ir))

      is AstStringLiteral ->
        ExpressionResult(entry, Value(strings[node]!!, MxStringType))

      is ThisLiteral -> ExpressionResult(
        entry,
        Value(LocalNamedIdentifier("this"), node.env.classEnv!!.type.ir),
      )

      is NewExpression -> when (node.typeId) {
        is AstIdentifier -> {
          val className = node.typeId.name
          val ty = node.type.ir
          val malloc = Call(
            entry.func.nextId("new.$className"),
            Value(
              GlobalNamedIdentifier("malloc"),
              FunctionType(listOf(Int32Type), ty),
            ),
            listOf(
              Value(
                IntegerLiteral((ty as PointerType).pointee!!.allocSize),
                Int32Type,
              ),
            ),
          )
          val ctor = CallVoid(
            Value(
              GlobalNamedIdentifier("$className.(init)"),
              FunctionType(listOf(PointerType(ty)), VoidType),
            ),
            listOf(malloc.value),
          )
          entry.body += listOf(malloc, ctor)
          ExpressionResult(entry, malloc.value)
        }

        is NewArrayType -> {
          val ty = node.type.ir
          val length = ir(entry, node.typeId.length)
          if (node.typeId.typeId is NewArrayType) {
            throw MxcInternalError(null, "Transform needed for multidim new[]")
          }
          val malloc = Call(
            entry.func.nextId("new.array"),
            Value(
              GlobalNamedIdentifier("array.new"),
              FunctionType(listOf(Int32Type), PointerType(ty)),
            ),
            listOf(length.result!!),
          )
          length.exit.body += malloc
          ExpressionResult(length.exit, malloc.value)
        }

        is HoleType -> error("unreachable")
      }

      is UnaryExpression -> {
        val argument = ir(entry, node.argument)
        val op = when (node.operator) {
          UnaryOperator.NOT -> IntBinaryOperation(
            entry.func.nextId("not"),
            BoolType,
            IntBinaryOperation.Op.XOR,
            argument.result!!.asType(),
            Value(IntegerLiteral(1), BoolType),
          )

          UnaryOperator.BIT_NOT -> IntBinaryOperation(
            entry.func.nextId("bit_not"),
            Int32Type,
            IntBinaryOperation.Op.XOR,
            argument.result!!.asType(),
            Value(IntegerLiteral(-1), Int32Type),
          )

          UnaryOperator.POS -> null
          UnaryOperator.NEG -> Int32BinaryOperation(
            entry.func.nextId("neg"),
            Int32BinaryOperation.Op.SUB,
            Value(IntegerLiteral(0), Int32Type),
            argument.result!!.asType(),
          )
        }
        op?.let { argument.exit.body.add(it) }
        ExpressionResult(argument.exit, op?.value ?: argument.result)
      }
    }

  private fun getLhsPointer(
    entry: BasicBlockContext,
    node: LeftHandSideExpression,
  ): ExpressionResult = when (node) {
    is ComputedMemberExpression -> {
      // Array = [ 0 x ptr ]
      val obj = ir(entry, node.`object`)
      val ix = ir(obj.exit, node.prop)
      val id = entry.func.nextId("computed")
      val op = GetElementPtr(
        id,
        obj.result!!.asType(),
        listOf(
          GepIndexLiteral(0),
          GepIndexValue(ix.result!!.asType()),
        ),
      )
      resFromOp(ix.exit, op)
    }

    is GroupExpression ->
      getLhsPointer(entry, node.expression as LeftHandSideExpression)

    is AstIdentifier -> ExpressionResult(entry, node.value)

    is MemberExpression -> {
      val obj = ir(entry, node.`object`)
      val ty = (node.`object`.type as MxStruct).ir as MxStructType
      val ix = ty.indexFromName[node.prop.name]!!
      val gep = GetElementPtr(
        entry.func.nextId("member.addr"),
        obj.result!!.asType(),
        listOf(GepIndexLiteral(0), GepIndexLiteral(ix)),
      )
      resFromOp(obj.exit, gep)
    }

    is PrefixUpdateExpression -> {
      val target = getLhsPointer(entry, node.argument)
      if (!ssa) TODO("ssa")
      val load =
        Load(entry.func.nextId("update.value"), target.result!!.asType())
      val res = Int32BinaryOperation(
        entry.func.nextId("update.result"),
        when (node.operator) {
          UpdateOperator.INC -> Int32BinaryOperation.Op.ADD
          UpdateOperator.DEC -> Int32BinaryOperation.Op.SUB
        },
        load.value.asType(),
        Value(IntegerLiteral(1), Int32Type),
      )
      val store = Store(target.result.operand, res.value)
      entry.body += listOf(load, res, store)
      target
    }
  }

  private val Type.allocSize: Int
    get() = when (this) {
      is AggregateType -> subtypes.fold(0) { size, ty -> size + ty.allocSize }
      is LocalNamedIdentifier -> typeFromStruct[name]!!.allocSize
      is PointerType, BoolType, CharType, Int32Type -> 4
      is FunctionType, is ArrayType, LabelType, VoidType -> error("unreachable")
    }
}

val BinaryOperator.ir32
  get() = when (this) {
    BinaryOperator.ADD -> Int32BinaryOperation.Op.ADD
    BinaryOperator.SUB -> Int32BinaryOperation.Op.SUB
    BinaryOperator.MUL -> Int32BinaryOperation.Op.MUL
    BinaryOperator.DIV -> Int32BinaryOperation.Op.SDIV
    BinaryOperator.REM -> Int32BinaryOperation.Op.SREM
    BinaryOperator.SHL -> Int32BinaryOperation.Op.SHL
    BinaryOperator.SHR -> Int32BinaryOperation.Op.ASHR
    else -> error("unreachable")
  }

val BinaryOperator.irBitwise
  get() = when (this) {
    BinaryOperator.BIT_AND -> IntBinaryOperation.Op.AND
    BinaryOperator.BIT_OR -> IntBinaryOperation.Op.OR
    BinaryOperator.BIT_XOR -> IntBinaryOperation.Op.XOR
    else -> error("unreachable")
  }

val BinaryOperator.irCompare
  get() = when (this) {
    BinaryOperator.EQ -> Icmp.Condition.EQ
    BinaryOperator.NE -> Icmp.Condition.NE
    BinaryOperator.LT -> Icmp.Condition.SLT
    BinaryOperator.LE -> Icmp.Condition.SLE
    BinaryOperator.GT -> Icmp.Condition.SGT
    BinaryOperator.GE -> Icmp.Condition.SGE
    else -> error("unreachable")
  }
