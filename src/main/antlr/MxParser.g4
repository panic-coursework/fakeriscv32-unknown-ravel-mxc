parser grammar MxParser;

options {
  tokenVocab=MxLexer;
}

program: programItem* EOF;

programItem
  : functionDeclaration # ProgramFunction
  | classDeclaration    # ProgramClass
  | lexicalDeclaration  # ProgramLexical
  ;

functionDeclaration: typeId identifier parameterList body=blockStatement;
parameterList: '(' (functionParameter ',')* functionParameter? ')';
functionParameter: typeId identifier;

classDeclaration: 'class' identifier '{' classElement* '}' ';';
classElement
  : lexicalDeclaration     # ClassLexical
  | constructorDeclaration # ClassConstructor
  | functionDeclaration    # ClassFunction
  ;
constructorDeclaration: identifier '(' ')' body=blockStatement;

lexicalDeclaration: typeId (lexicalBinding ',')* lexicalBinding eos;
lexicalBinding: identifier ('=' initializer=expression)?;

statement
  : blockStatement      # StmtBlock
  | expressionStatement # StmtExpression
  | ifStatement         # StmtIf
  | iterationStatement  # StmtIteration
  | continueStatement   # StmtContinue
  | breakStatement      # StmtBreak
  | returnStatement     # StmtReturn
  | emptyStatement      # StmtEmpty
  | lexicalDeclaration  # StmtLexicalDeclaration
  ;
blockStatement: '{' statement* '}';

expressionStatement: expression eos;
ifStatement: 'if' '(' cond=expression ')' consequent=statement ('else' alternate=statement)?;
iterationStatement
  : 'while' '(' cond=expression ')' body=statement # LoopWhile
  | 'for' '(' init=expression? ';'    cond=expression? ';' step=expression? ')' body=statement # LoopForExpr
  | 'for' '(' init=lexicalDeclaration cond=expression? ';' step=expression? ')' body=statement # LoopForDecl
  ;
continueStatement: 'continue' eos;
breakStatement: 'break' eos;
returnStatement: 'return' value=expression? eos;
emptyStatement: eos;

eos: ';';

literalExpression
  : integerLiteral # LitInteger
  | stringLiteral  # LitString
  | thisLiteral    # LitThis
  | booleanLiteral # LitBoolean
  | nullLiteral    # LitNull
  ;

leftHandSideExpression
  : identifier                                                   # LhsIdentifier
  | literalExpression                                            # LhsLiteral
  | '[' capture='&'? ']' parameterList? '->' body=blockStatement # LhsLambda
  | 'new' type=newTypeId ('(' ')')?                              # LhsNew
  | '(' expression ')'                                           # LhsParenthesized
  | object=leftHandSideExpression '.' prop=identifier            # LhsMember
  | object=leftHandSideExpression '[' prop=expression ']'        # LhsComputed
  | op=('++' | '--') target=leftHandSideExpression               # LhsPrefixUpdate
  | callee=leftHandSideExpression arguments                      # LhsCall
  ;

expression
  : l=leftHandSideExpression                                             # ExprLhs
  | target=leftHandSideExpression op=('++' | '--')                       # ExprPostfixUpdate
  | <assoc=right> op=('!' | '~' | '+' | '-')                r=expression # ExprUnary
  | l=expression op=('*' | '/' | '%')                       r=expression # ExprBinary
  | l=expression op=('+' | '-')                             r=expression # ExprBinary
  | l=expression op=('<<' | '>>')                           r=expression # ExprBinary
  | l=expression op=('<=' | '>=' | '<' | '>' | '==' | '!=') r=expression # ExprBinary
  | l=expression op='&'                                     r=expression # ExprBinary
  | l=expression op='|'                                     r=expression # ExprBinary
  | l=expression op='^'                                     r=expression # ExprBinary
  | l=expression op='&&'                                    r=expression # ExprBinary
  | l=expression op='||'                                    r=expression # ExprBinary
  | <assoc=right> l=leftHandSideExpression op='='           r=expression # ExprAssign
  ;

arguments: '(' (args=expression ',')* args=expression? ')';
integerLiteral
  : DecimalIntegerLiteral # IntDecimal
  | HexIntegerLiteral     # IntHex
  | BinaryIntegerLiteral  # IntBinary
  ;
stringLiteral: StringLiteral;
thisLiteral: 'this';
booleanLiteral: 'true' | 'false';
nullLiteral: 'null';

identifier: IdentifierName;

typeId
  : primitiveTypeId # TypePrimitive
  | '_'             # TypeHole
  | identifier      # TypeUserDefined
  | typeId '[' ']'  # TypeArray
  | '(' typeId ')'  # TypeParenthesized
  | <assoc=right> param=typeId '->' returnType=typeId # TypeFunctionSingleParam
  | params=parameterTypeList   '->' returnType=typeId # TypeFunction
  ;
primitiveTypeId: 'bool' | 'int' | 'void' | 'string';
parameterTypeList: '(' (typeId ',')* typeId? ')';

newTypeIdBasic
  : primitiveTypeId # NewPrimitive
  | identifier      # NewIdentifier
  ;
newTypeIdArray
  : t=newTypeIdBasic '[' length=expression ']'  # NewArrayStart
  | t=newTypeIdArray '[' length=expression? ']' # NewArrayPart
  ;
newTypeId
  : newTypeIdArray # NewArray
  | newTypeIdBasic # NewBasic
  ;
