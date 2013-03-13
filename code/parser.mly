(* This file defines the parser. *)

%{

  open MIPSOps
  open LPP
  open Primitive

%}

(* ------------------------------------------------------------------------- *)
(* This is the start symbol of the grammar. *)

%start <LPP.program> program

(* ------------------------------------------------------------------------- *)
(* These tokens are produced by the lexer and exploited by the parser. *)

%token <int32> INTCONST
%token <bool> BOOLCONST
%token <string Location.t> ID
%token PLUS MINUS TIMES SLASH AND OR NOT LT LE GT GE EQ NE
%token LPAREN RPAREN LBRACKET RBRACKET COMMA COLONEQ SEMICOLON COLON DOT
%token PROGRAM BEGIN END IF THEN ELSE WHILE DO PROCEDURE FUNCTION VAR
%token NEW READLN WRITE WRITELN LENGTH
%token INTEGER BOOLEAN ARRAY OF

(* ------------------------------------------------------------------------- *)
(* Now come the precedence levels. These directives are used to resolve
   a number of shift/reduce and reduce/reduce conflicts. *)

(* The tokens with lower precedence come first in this list. In other words,
   the fact that [OR] comes before [AND] in the list means that [OR] has lower
   precedence than [AND]. *)

(* The fact that [OR] has lower precedence than [AND] means that the condition
   [x or y and z] is interpreted as [x or (y and z)]. In other words, in this
   case, a shift/reduce conflict is resolved in favor of shifting. *)

(* The fact that [OR] is declared to be left-associative means that [x or y or z]
   is interpreted as [(x or y) or z]. In other words, in this case, a
   shift/reduce conflict is resolved in favor of reduction. *)

(* The fact that [LT] is declared to be non-associative means that [x < y < z]
   is rejected: it causes a syntax error. *)

(* The fact that [PLUS] has lower precedence than [LBRACKET] means that
   the expression [x + y[z]] is interpreted as [x + (y[z])], as desired. *)

(* The instruction [if c1 then if c2 then i1 else i2] involves a classic
   shift/reduce conflict, known as the dangling-else conflict. The conflict
   occurs when the token [ELSE] is discovered. At this point, reducing the
   production [instruction -> IF condition THEN instruction] leads
   to interpreting this instruction as [if c1 then (if c2 then i1) else i2],
   while shifting the token [ELSE] leads to interpreting it as
   [if c1 then (if c2 then i1 else i2)]. The desired behavior is the latter,
   so we must resolve the conflict in favor of shifting. By default, the
   precedence level associated with reducing the above production is the
   level of the token [THEN]. (This convention is explained in Menhir's
   manual. It is inherited from yacc.) So, we give [THEN] a lower precedence
   level than [ELSE]. This is done by the last two lines in the declarations
   that follow. *)

(* The declaration [%nonassoc unary_minus] makes [unary_minus] a pseudo-token:
   it is not an actual token, but only a name for a certain precedence level.
   This name is later used in an annotation of the form [%prec unary_minus],
   which assigns a precedence level to the reduction of the production
   [expression -> MINUS expression]. The fact that [unary_minus] has greater
   precedence than [PLUS], for instance, means that [-x+y] is correctly
   interpreted as [(-x)+y]. *)

(* When applicable, this mechanism for defining precedence levels is extremely
   concise: in this grammar, for instance, more than a hundred conflicts are
   resolved by the ten lines that follow. *)

%left OR
%left AND
%nonassoc NOT
%nonassoc LT LE GT GE EQ NE
%left MINUS PLUS
%left TIMES SLASH
%nonassoc unary_minus
%nonassoc LBRACKET
%nonassoc THEN
%nonassoc ELSE

%%

(* The productions of the grammar follow. *)

(* ------------------------------------------------------------------------- *)
(* A generic definition for recognizing a syntactic element X and tagging
   its semantic value with a source code location. *)

located(X):
  x = X
  { Location.make $startpos $endpos x }

(* ------------------------------------------------------------------------- *)
(* Types. *)

typ:
| INTEGER
    { TypInt }
| BOOLEAN
    { TypBool }
| ARRAY OF t = typ
    { TypArray t }

(* ------------------------------------------------------------------------- *)
(* Expressions. *)

(* Formal and actual parameter lists are delimited with parentheses
   and separated with semicolons and commas, respectively. *)

expression:
  e = located(raw_expression)
    { e }
| LPAREN e = expression RPAREN
    { e }

raw_expression:
  i = INTCONST
    { EConst (ConstInt i) }
| b = BOOLCONST
    { EConst (ConstBool b) }
| id = ID
    { EGetVar id }
| MINUS e = expression %prec unary_minus
    { EUnOp (UOpNeg, e) }
| e1 = expression op = binop e2 = expression
    { EBinOp (op, e1, e2) }
| c = callee LPAREN actuals = separated_list(COMMA, expression) RPAREN
    { EFunCall (c, actuals) }
| a = expression LBRACKET i = expression RBRACKET
    { EArrayGet (a, i) }
| NEW ARRAY OF t = typ LBRACKET e = expression RBRACKET
    { EArrayAlloc (t, e) }
| LENGTH LPAREN e = expression RPAREN
    { EArrayLength e }

(* Binary operators are defined as a separate nonterminal symbol
   for enhanced readability. However, note that this symbol is
   declared %inline, which means that its definition is in fact
   textually expanded into the definition of expressions above.
   This is necessary for operator priorities to be properly
   taken into account. *)

%inline binop:
| PLUS
    { OpAdd }
| MINUS
    { OpSub }
| TIMES
    { OpMul }
| SLASH
    { OpDiv }
| LT
    { OpLt }
| LE
    { OpLe }
| GT
    { OpGt }
| GE
    { OpGe }
| EQ
    { OpEq }
| NE
    { OpNe }

(* ------------------------------------------------------------------------- *)
(* Conditions.

   We first define ``nontrivial'' conditions, which cannot simply be an
   expression or a parenthesized expression: they must involve at least
   one of the Boolean operators NOT, AND, OR. Then, we define a condition
   to be either a (Boolean-valued) expression or a nontrivial condition.

   This two-step approach is necessary to avoid ambiguity with
   parentheses. Indeed, conditions include expressions, and both
   conditions and expressions can be parenthesized, so a naïve
   approach would yield an ambiguous grammar. Thanks to Yannick Moy
   for suggesting this solution. *)

nontrivial_condition:
| NOT c = condition
    { CNot c }
| c1 = condition AND c2 = condition
    { CAnd (c1, c2) }
| c1 = condition OR c2 = condition
    { COr (c1, c2) }
| LPAREN c = nontrivial_condition RPAREN
    { c }

condition:
| e = expression
    { CExpression e }
| c = nontrivial_condition
    { c }

(* ------------------------------------------------------------------------- *)
(* Instructions and blocks. *)

instruction:
| b = block
    { b }
| id = ID COLONEQ e = expression
    { ISetVar (id, e) }
| callee = callee LPAREN actuals = separated_list(COMMA, expression) RPAREN
    { IProcCall (callee, actuals) }
| a = expression LBRACKET i = expression RBRACKET COLONEQ e = expression
    { IArraySet (a, i, e) }
| IF c = condition THEN b = instruction
    { IIf (c, b, ISeq []) }
| IF c = condition THEN b1 = instruction ELSE b2 = instruction
    { IIf (c, b1, b2) }
| WHILE c = condition DO b = instruction
    { IWhile (c, b) }

block:
| BEGIN is = separated_list(SEMICOLON, instruction) END
    { ISeq is }

(* ------------------------------------------------------------------------- *)
(* Callees. *)

%inline callee:
  c = located(raw_callee)
    { c }

raw_callee:
| WRITE
    { CPrimitiveFunction Write }
| WRITELN
    { CPrimitiveFunction Writeln }
| READLN
    { CPrimitiveFunction Readln }
| id = ID
    { CUserFunction (Location.content id)}

(* ------------------------------------------------------------------------- *)
(* Procedures and functions. *)

(* Procedures and functions are very similar, so we are able to
   express a single semantic action that covers both productions. In
   the case of procedures, the variable [result] is bound to the value
   [None] via a simple trick: we recognize the non-terminal
   [no_result_type], whose language is empty. *)

procedure:
| FUNCTION f = ID
  LPAREN formals = separated_bindings RPAREN
  COLON result = some_result_type SEMICOLON
  locals = variables
  body = block
  SEMICOLON

| PROCEDURE f = ID
  LPAREN formals = separated_bindings RPAREN
  result = no_result_type SEMICOLON
  locals = variables
  body = block
  SEMICOLON

    {
      f, {
        formals = formals;
        result = result;
        locals = locals;
        body = body
      }
    }

%inline no_result_type:
  (* nothing *)
    { None }

%inline some_result_type:
  t = typ
    { Some t }

separated_bindings:
| bindings = separated_list(SEMICOLON, binding) (* list can be empty *)
    { List.flatten bindings }

terminated_bindings:
| bindings = terminated(binding, SEMICOLON)+    (* list is nonempty *)
    { List.flatten bindings }

binding:
| ids = separated_nonempty_list(COMMA, ID) COLON t = typ
    { List.map (fun id -> (id, t)) ids }

variables:
| vars = loption(preceded(VAR, terminated_bindings))
    { vars }

(* ------------------------------------------------------------------------- *)
(* Programs. *)

program:
PROGRAM
globals = variables
defs = procedure*
main = block
DOT
    {{
      globals = globals;
      defs = defs;
      main = main
    }}

