lexer grammar MxLexer;

MultiLineComment: '/*' .*? '*/' -> channel(HIDDEN);
SingleLineComment: '//' ~[\r\n\u2028\u2029]* -> channel(HIDDEN);

Void: 'void';
Bool: 'bool';
Int: 'int';
String: 'string';
New: 'new';
Class: 'class';
Null: 'null';
True: 'true';
False: 'false';
This: 'this';
If: 'if';
Else: 'else';
For: 'for';
While: 'while';
Break: 'break';
Continue: 'continue';
Return: 'return';

IdentifierName: IdentifierStart IdentifierPart*;
fragment IdentifierStart: [a-zA-Z];
fragment IdentifierPart: IdentifierStart | [0-9_];

fragment NumericLiteralSeparator: '\'';
DecimalIntegerLiteral
  : '0' [dD] DecimalDigits
  | [1-9] DecimalDigitsPart
  | '0'
  ;
fragment DecimalDigitsPart: (DecimalDigit | NumericLiteralSeparator)*;
fragment DecimalDigits: DecimalDigit DecimalDigitsPart;
fragment DecimalDigit: [0-9];
HexIntegerLiteral: '0' [xX] HexDigits;
fragment HexDigits: HexDigit (HexDigit | NumericLiteralSeparator)*;
fragment HexDigit: [0-9a-fA-F];
BinaryIntegerLiteral: '0' [bB] BinaryDigits;
fragment BinaryDigits: BinaryDigit (BinaryDigit | NumericLiteralSeparator)*;
fragment BinaryDigit: [01];

StringLiteral: '"' StringChars '"';
fragment StringChars: StringChar*?;
fragment StringChar
  : ~["\\\n]
  | '\\' EscapeSequence
  ;
fragment EscapeSequence
  : CharacterEscapeSequence
  | HexEscapeSequence
  | UnicodeEscapeSequence
  ;
fragment CharacterEscapeSequence: ['"\\brntfv0$];
fragment HexEscapeSequence: [xX] HexDigit HexDigit;
fragment UnicodeEscapeSequence
  : [uU] HexDigit HexDigit HexDigit HexDigit
  | [uU] '{' HexDigit HexDigit+ '}'
  ;

Arrow: '->';

Inc: '++';
Dec: '--';
Add: '+';
Sub: '-';
Mul: '*';
Div: '/';
Mod: '%';
And: '&&';
Or: '||';
Not: '!';
Shl: '<<';
Shr: '>>';
BitAnd: '&';
BitOr: '|';
BitNot: '~';
BitXor: '^';
Ge: '>=';
Le: '<=';
Gt: '>';
Lt: '<';
Eq: '==';
Ne: '!=';
Assign: '=';
PropAccess: '.';
BracketOpen: '[';
BracketClose: ']';
ParenOpen: '(';
ParenClose: ')';
BraceOpen: '{';
BraceClose: '}';
Semicolon: ';';
Comma: ',';

Hole: '_';

WS
  :(' '
  | '\t'
  | '\u000B'
  | '\u000C'
  | '\u00A0'
  ) -> channel(HIDDEN);
LineTerminator
  :('\r'
  | '\n'
  | '\u2028'
  | '\u2029'
  ) -> channel(HIDDEN);
