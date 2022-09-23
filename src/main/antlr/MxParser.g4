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

functionDeclaration: typeId name=identifier parameterList body=blockStatement;
parameterList: '(' (functionParameter ',')* functionParameter? ')';
functionParameter: typeId name=identifier;

classDeclaration: 'class' name=identifier '{' classElement* '}' ';';
classElement
  : lexicalDeclaration     # ClassLexical
  | constructorDeclaration # ClassConstructor
  | functionDeclaration    # ClassFunction
  ;
constructorDeclaration: name=identifier '(' ')' body=blockStatement;

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
  | 'for' '(' init=expression? ';' cond=expression? ';' step=expression? ')' body=statement   # LoopForExpr
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
  | 'new' type=newableType ('(' ')')?                            # LhsNew
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
  | identifier      # TypeUserDefined
  | typeId '[' ']'  # TypeArray
  ;
primitiveTypeId: 'bool' | 'int' | 'void' | 'string';

newableTypeWithoutLength
  : primitiveTypeId # NewPrimitive
  | identifier      # NewIdentifier
  ;
newableTypeWithLength
  : t=newableTypeWithoutLength '[' length=expression ']' # NewArrayStart
  | t=newableTypeWithLength '[' length=expression? ']'   # NewArrayPart
  ;
newableType
  : newableTypeWithLength    # NewArray
  | newableTypeWithoutLength # NewBasic
  ;
