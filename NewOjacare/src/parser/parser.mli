type token =
  | PACKAGE
  | CLASS
  | INTERFACE
  | EXTENDS
  | IMPLEMENTS
  | STATIC
  | ABSTRACT
  | FINAL
  | NAME
  | CALLBACK
  | VOID
  | BOOLEAN
  | BYTE
  | SHORT
  | CAMLINT
  | INT
  | LONG
  | FLOAT
  | DOUBLE
  | CHAR
  | STRING
  | TOP
  | ARRAY
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | COMMA
  | SEMI
  | COLON
  | DOT
  | INIT
  | IDENT of (string)
  | EOF

val idl_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Idl.file
