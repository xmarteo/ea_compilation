exception Error

type token = 
  | WRITELN
  | WRITE
  | WHILE
  | VAR
  | TIMES
  | THEN
  | SLASH
  | SEMICOLON
  | RPAREN
  | READLN
  | RBRACKET
  | PROGRAM
  | PROCEDURE
  | PLUS
  | OR
  | OF
  | NOT
  | NEW
  | NE
  | MINUS
  | LT
  | LPAREN
  | LENGTH
  | LE
  | LBRACKET
  | INTEGER
  | INTCONST of (int32)
  | IF
  | ID of (string Location.t)
  | GT
  | GE
  | FUNCTION
  | EQ
  | END
  | ELSE
  | DOT
  | DO
  | COMMA
  | COLONEQ
  | COLON
  | BOOLEAN
  | BOOLCONST of (bool)
  | BEGIN
  | ARRAY
  | AND


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (LPP.program)