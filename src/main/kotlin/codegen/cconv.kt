package org.altk.lab.mxc.codegen

class StackFrame {
}

val wordSize = 4

fun alignFrame(x: Int) = (x + 15) / 16 * 16

val callerSaveRegs = listOf(
  Pair(1, "ra"),
  Pair(5, "t0"),
  Pair(6, "t1"),
  Pair(7, "t2"),
  Pair(10, "a0"),
  Pair(11, "a1"),
  Pair(12, "a2"),
  Pair(13, "a3"),
  Pair(14, "a4"),
  Pair(15, "a5"),
  Pair(16, "a6"),
  Pair(17, "a7"),
  Pair(28, "t3"),
  Pair(29, "t4"),
  Pair(30, "t5"),
  Pair(31, "t6"),
)

val calleeSaveRegs = listOf(
  Pair(8, "s0"),
  Pair(9, "s1"),
  Pair(18, "s2"),
  Pair(19, "s3"),
  Pair(20, "s4"),
  Pair(21, "s5"),
  Pair(22, "s6"),
  Pair(23, "s7"),
  Pair(24, "s8"),
  Pair(25, "s9"),
  Pair(26, "s10"),
  Pair(27, "s11"),
)
