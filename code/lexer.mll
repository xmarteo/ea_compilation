{

  (* This module implements a lexical analysis for Pseudo-Pascal.
     Note that, contrary to Pascal, it is case-sensitive. *)

  open Lexing
  open Parser

  (* Updates the line counter, which is used in some error messages. *)

  let update_loc lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  (* Signals an error at the current token. *)

  let error lexbuf msg =
    Error.error2 (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) msg

}

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]

let digit = [ '0'-'9' ]

let integer = ( "0x" | "0o" | "0b" )? digit+

let identifier = [ 'A'-'Z' 'a'-'z' ]+ digit*

rule main = parse
| newline
    { update_loc lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| integer as i
    {
      try
        INTCONST (Int32.of_string i)
      with Failure _ ->
	error lexbuf "Invalid integer constant."
    }
| "true"
    { BOOLCONST true }
| "false"
    { BOOLCONST false }
| "+"
    { PLUS }
| "-"
    { MINUS }
| "*"
    { TIMES }
| "/"
    { SLASH }
| "and"
    { AND }
| "or"
    { OR }
| "not"
    { NOT }
| "<"
    { LT }
| "<="
    { LE }
| ">"
    { GT }
| ">="
    { GE }
| "="
    { EQ }
| "<>"
    { NE }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "["
    { LBRACKET }
| "]"
    { RBRACKET }
| ","
    { COMMA }
| ":="
    { COLONEQ }
| ";"
    { SEMICOLON }
| ":"
    { COLON }
| "."
    { DOT }
| "program"
    { PROGRAM }
| "begin"
    { BEGIN }
| "end"
    { END }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "while"
    { WHILE }
| "do"
    { DO }
| "procedure"
    { PROCEDURE }
| "function"
    { FUNCTION }
| "var"
    { VAR }
| "new"
    { NEW }
| "readln"
    { READLN }
| "write"
    { WRITE }
| "writeln"
    { WRITELN }
| "integer"
    { INTEGER }
| "length"
    { LENGTH }
| "cast"
    { CAST }
| "boolean"
    { BOOLEAN }
| "array"
    { ARRAY }
| "of"
    { OF }
| identifier as id
    { ID (Location.make (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) id) }
| "{"
    { comment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| eof
    { error lexbuf "Lexical error: unexpected end of file." }
| _
    { error lexbuf "Lexical error: unexpected character(s)." }

and comment openingp = parse
| "{"
    { comment (lexeme_start_p lexbuf) lexbuf; comment openingp lexbuf }
| "}"
    { () }
| newline
    { update_loc lexbuf; comment openingp lexbuf }
| eof
    { Error.error2 openingp (lexeme_start_p lexbuf) "Unterminated comment." }
| _
    { comment openingp lexbuf }

