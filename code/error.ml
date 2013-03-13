open Printf
open Lexing

let log verbosity msg =
  if Settings.verbose >= verbosity then
    fprintf stderr "%t%!" msg

let positions pos1 pos2 =
  let file = pos1.pos_fname in
  let line = pos1.pos_lnum in
  let char1 = pos1.pos_cnum - pos1.pos_bol in
  let char2 = pos2.pos_cnum - pos1.pos_bol in (* intentionally [pos1.pos_bol] *)
  fprintf stderr "File \"%s\", line %d, characters %d-%d:\n" file line (char1 + 1) (char2 + 1)

let errors tvs message =
  List.iter (fun tv ->
    positions (Location.startpos tv) (Location.endpos tv);
  ) tvs;
  fprintf stderr "%s\n%!" message;
  exit 1

let error tv message =
  errors [ tv ] message

let error2 pos1 pos2 message =
  error (Location.make pos1 pos2 ()) message

