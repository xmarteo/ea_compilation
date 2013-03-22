let interpretif b f x =
  if b then begin
    f x;
    exit 0
  end
  else
    x

let program : LPP.program =
  let channel = open_in Settings.filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.Lexing.lex_curr_p <-
      { 
	Lexing.pos_fname = Settings.filename; 
	Lexing.pos_lnum  = 1;
	Lexing.pos_bol   = 0; 
	Lexing.pos_cnum  = 0
      };
  try
    let program = Parser.program Lexer.main lexbuf in
    close_in channel;
    program
  with Parser.Error ->
    Error.error2
      (Lexing.lexeme_start_p lexbuf)
      (Lexing.lexeme_end_p lexbuf)
      "Syntax error."

let () =
  Typechecking.typecheck_program program

let program : PP.program =
  interpretif Settings.ipp InterpretPP.interpret (
    Print.showif Settings.dpp PrintPP.print_program  (
      Lpp2pp.translate_program program
    )
  )

let program : UPP.program =
  interpretif Settings.iupp InterpretUPP.interpret (
    Print.showif Settings.dupp PrintUPP.print_program (
      Pp2upp.translate_program program
    )
  )

let program : RTL.program =
  interpretif Settings.irtl InterpretRTL.interpret (
    Print.showif Settings.drtl PrintRTL.print_program (
      Upp2rtl.translate_program program
    )
  )

let program : RTL.program =
  if Settings.nocse then
    program
  else
    interpretif Settings.icse InterpretRTL.interpret (
      Print.showif Settings.dcse PrintRTL.print_program (
	Cse.translate_program program
      )
    )

let program : ERTL.program =
  interpretif Settings.iertl InterpretERTL.interpret (
    Print.showif Settings.dertl (PrintERTL.print_program Settings.dlive) (
      Rtl2ertl.translate_program program
    )
  )

let () =
  Option.iter (fun f ->
    try
      let proc : ERTL.procedure = StringMap.find f program.ERTL.defs in
      let _, graph = Build.build proc in
      Interference.print stdout graph
    with Not_found ->
      Printf.fprintf stderr "Warning: there is no procedure named %s.\n%!" f
  ) Settings.dgraph

let program : LTL.program =
  interpretif Settings.iltl InterpretLTL.interpret (
    Print.showif Settings.dltl PrintLTL.print_program (
      Ertl2ltl.translate_program program
    )
  )

let program : LIN.program =
  interpretif Settings.ilin InterpretLIN.interpret (
    Print.showif Settings.dlin PrintLIN.print_program (
      Ltl2lin.translate_program program
    )
  )

let program : ASM.program =
  Print.showif Settings.dasm PrintASM.print_program (
    Lin2asm.translate_program program
  )

let () =
  let output = open_out (Filename.chop_extension Settings.filename ^ ".spi") in
  Printf.fprintf output "%s%!" (PrintASM.print_program () program);
  close_out output

