let rec translate_expression e =
  translate_raw_expression (Location.content e)

and translate_raw_expression = function
  | LPP.EConst c ->
      PP.EConst c
  | LPP.EGetVar id ->
      PP.EGetVar (Location.content id)
  | LPP.EUnOp (op, e) ->
      PP.EUnOp (op, translate_expression e)
  | LPP.EBinOp (op, e1, e2) ->
      PP.EBinOp (op, translate_expression e1, translate_expression e2)
  | LPP.EFunCall (callee, es) ->
      PP.EFunCall (Location.content callee, List.map translate_expression es)
  | LPP.EArrayGet (e1, e2) ->
      PP.EArrayGet (translate_expression e1, translate_expression e2)
  | LPP.EArrayAlloc (t, e) ->
      PP.EArrayAlloc (t, translate_expression e)
  | LPP.EArrayLength t ->
      PP.EArrayLength (translate_expression t)

let rec translate_condition = function
  | LPP.CExpression e ->
      PP.CExpression (translate_expression e)
  | LPP.CNot c ->
      PP.CNot (translate_condition c)
  | LPP.CAnd (c1, c2) ->
      PP.CAnd (translate_condition c1, translate_condition c2)
  | LPP.COr (c1, c2) ->
      PP.COr (translate_condition c1, translate_condition c2)

let rec translate_instruction = function
  | LPP.IProcCall (callee, es) ->
      PP.IProcCall (Location.content callee, List.map translate_expression es)
  | LPP.ISetVar (id, e) ->
      PP.ISetVar (Location.content id, translate_expression e)
  | LPP.IArraySet (e1, e2, e3) ->
      PP.IArraySet (translate_expression e1, translate_expression e2, translate_expression e3)
  | LPP.ISeq is ->
      PP.ISeq (List.map translate_instruction is)
  | LPP.IIf (c, i1, i2) ->
      PP.IIf (translate_condition c, translate_instruction i1, translate_instruction i2)
  | LPP.IWhile (c, i) ->
      PP.IWhile (translate_condition c, translate_instruction i)

let translate_binding (id, t) =
  (Location.content id, t)

let translate_bindings bindings =
  StringMap.of_association_list (List.map translate_binding bindings)

let translate_procedure proc =
  {
    PP.formals = List.map translate_binding proc.LPP.formals;
    PP.result = proc.LPP.result;
    PP.locals = translate_bindings proc.LPP.locals;
    PP.body = translate_instruction proc.LPP.body
  }

let translate_program p =
  {
    PP.globals = translate_bindings p.LPP.globals;
    PP.defs = StringMap.map translate_procedure (translate_bindings p.LPP.defs);
    PP.main = translate_instruction p.LPP.main
  }

