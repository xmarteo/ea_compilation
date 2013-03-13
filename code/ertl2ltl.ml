open Integer

let translate_procedure f (proc : ERTL.procedure) : LTL.procedure =

  (* Allocate a reference that will hold the control flow
     graph. Define a function that generates an instruction at a fresh
     label. *)

  let graph, generate =
    Label.Map.generator proc.ERTL.luniverse
  in

  (* Build an interference graph for this procedure, and color
     it. Define a function that allows consulting the coloring. *)

  let module G = struct
    let liveafter, graph = Build.build proc
    let uses = Uses.examine_procedure proc
    let verbose = (Settings.dcolor = Some f)
    let () =
      if verbose then
	Printf.printf "Starting hardware register allocation for %s.\n" f
  end in

  let module C = Coloring.Color (G) in

  let lookup r =
    Interference.Vertex.Map.find (Interference.lookup G.graph r) C.coloring
  in

  (* Restrict the interference graph to concern spilled vertices only,
     and color it again, this time using stack slots as colors. *)

  let module H = struct
    let graph = Interference.droph (Interference.restrict G.graph (fun v ->
      match Interference.Vertex.Map.find v C.coloring with
      | Coloring.Spill ->
	  true
      | Coloring.Color _ ->
	  false
    ))
    let verbose = (Settings.dspill = Some f)
    let () =
      if verbose then
	Printf.printf "Starting stack slot allocation for %s.\n" f
  end in

  let module S = Spill.Color (H) in

  (* Define a new function that consults both colorings at once. *)

  let lookup r =
    match lookup r with
    | Coloring.Spill ->
	Ertl2ltlI.Spill (Interference.Vertex.Map.find (Interference.lookup H.graph r) S.coloring)
    | Coloring.Color color ->
	Ertl2ltlI.Color color
  in

  (* We are now ready to instantiate the functor that deals with the
     translation of instructions. The reason why we make this a
     separate functor is purely pedagogical. Smaller modules are
     easier to understand. *)

  let module I = Ertl2ltlI.Make (struct
    let lookup = lookup
    let generate = generate
  end) in

  (* Translate the instructions in the existing control flow graph.
     Pure instructions whose destination pseudo-register is dead are
     eliminated on the fly. *)

  let () =
    Label.Map.iter (fun label instruction ->
      let instruction =
	match Liveness.eliminable (G.liveafter label) instruction with
	| Some successor ->
	    LTL.IGoto successor
	| None ->
	    I.translate_instruction instruction
      in
      graph := Label.Map.add label instruction !graph
    ) proc.ERTL.graph
  in

  (* Build an [LTL] procedure. *)

  {
    LTL.formals = proc.ERTL.formals;
    LTL.locals = S.locals;
    LTL.luniverse = proc.ERTL.luniverse;
    LTL.entry = proc.ERTL.entry;
    LTL.graph = !graph
  }

let translate_program (p : ERTL.program) : LTL.program = {
  LTL.globals = p.ERTL.globals;
  LTL.defs = StringMap.mapi translate_procedure p.ERTL.defs
}

