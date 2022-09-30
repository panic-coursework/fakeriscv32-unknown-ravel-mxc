package org.altk.lab.mxc.ast

import org.altk.lab.mxc.BuiltinSourceContext

fun injectReturnZeroToMain(program: Program) =
  Program(program.ctx, program.body.map { decl ->
    if (decl is FunctionDeclaration && decl.id.name == "main") {
      val ret = ReturnStatement(
        BuiltinSourceContext,
        IntegerLiteral(BuiltinSourceContext, 0),
      )
      FunctionDeclaration(
        decl.ctx, decl.id, decl.params, decl.returnType,
        BlockStatement(decl.body.ctx, decl.body.body + ret),
      )
    } else {
      decl
    }
  })
