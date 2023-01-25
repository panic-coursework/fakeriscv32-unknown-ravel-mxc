package org.altk.lab.mxc.codegen

const val wordSize = 4

fun alignFrame(x: Int) = (x + 15) / 16 * 16

val callerSaveRegs =
  (listOf("ra") + (0..6).map { "t$it" } + (0..7).map { "a$it" }).map { it.R }

val calleeSaveRegs = (0..11).map { "s$it".R }

val allocatableRegs = (0..6).map { "t$it".R } + (0..7).map { "a$it".R } + (0..11).map { "s$it".R }
