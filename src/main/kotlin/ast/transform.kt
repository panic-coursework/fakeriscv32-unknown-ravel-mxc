package org.altk.lab.mxc.ast

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
