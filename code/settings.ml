(* Prepare for parsing the command line. *)

let dpp, dupp, drtl, dcse, dertl, dltl, dlin, dasm =
  ref false, ref false, ref false, ref false, ref false, ref false, ref false, ref false

let ipp, iupp, irtl, icse, iertl, iltl, ilin =
  ref false, ref false, ref false, ref false, ref false, ref false, ref false

let dlive, few, nocse =
  ref false, ref false, ref false

let dgraph, dcolor, dspill =
  ref None, ref None, ref None

let delimit =
  ref false

let set o f =
  o := Some f

let verbose =
  ref 0

let filename = 
  ref None

let insert name = 
  filename := Some name

let options = Arg.align [
  "-delimit", Arg.Set delimit, " (undocumented)";
  "-dpp", Arg.Set dpp, " Display the PP program";
  "-dupp", Arg.Set dupp, " Display the UPP program";
  "-drtl", Arg.Set drtl, " Display the RTL program";
  "-dcse", Arg.Set dcse, " Display the RTL program after CSE";
  "-dertl", Arg.Set dertl, " Display the ERTL program";
  "-dltl", Arg.Set dltl, " Display the LTL program";
  "-dlin", Arg.Set dlin, " Display the LIN program";
  "-dasm", Arg.Set dasm, " Display the ASM program";
  "-dlive", Arg.Set dlive, " Display liveness information of top of ERTL program";
  "-dgraph", Arg.String (set dgraph), "<f> Display an interference graph for function <f>";
  "-dcolor", Arg.String (set dcolor), "<f> Comment while allocating registers for function <f>";
  "-dspill", Arg.String (set dspill), "<f> Comment while allocating stack slots for function <f>";
  "-ipp", Arg.Set ipp, " Interpret the PP program";
  "-iupp", Arg.Set iupp, " Interpret the UPP program";
  "-irtl", Arg.Set irtl, " Interpret the RTL program";
  "-icse", Arg.Set icse, " Interpret the RTL program after CSE";
  "-iertl", Arg.Set iertl, " Interpret the ERTL program";
  "-iltl", Arg.Set iltl, " Interpret the LTL program";
  "-ilin", Arg.Set ilin, " Interpret the LIN program";
  "-nocse", Arg.Set nocse, " Do not perform CSE";
  "-few", Arg.Set few, " Pretend there are fewer hardware registers";
  "-v", Arg.Set_int verbose, "<level> Sets verbosity level";
]

let usage =
  Printf.sprintf "Usage: %s <options> <filename>" Sys.argv.(0)

(* Parse the command line. *)

let () =
  Arg.parse options insert usage

(* Export the settings. *)

let dpp, dupp, drtl, dcse, dertl, dltl, dlin, dasm =
  !dpp, !dupp, !drtl, !dcse, !dertl, !dltl, !dlin, !dasm

let ipp, iupp, irtl, icse, iertl, iltl, ilin =
  !ipp, !iupp, !irtl, !icse, !iertl, !iltl, !ilin

let dlive, few, nocse, dgraph, dcolor, dspill =
  !dlive, !few, !nocse, !dgraph, !dcolor, !dspill

let delimit = 
  !delimit

let delimit name body =
  if delimit then
    Printf.sprintf "# begin %s \n%s\n# end %s" name body name
  else
    body

let verbose =
  !verbose

let filename =
  match !filename with
  | None ->
      Arg.usage options usage;
      exit 1
  | Some filename ->
      filename

