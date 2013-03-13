(* This module translates [UPP] into [RTL]. It relies on [Upp2rtlI]
   for translating expressions, conditions, and instructions. It deals
   with translating procedures (which involves creating the
   environment module required by [Upp2rtlI]) and programs. *)

(* ------------------------------------------------------------------------- *)

(* Translating procedures.

   [translate_procedure f proc] translates a procedure whose name is
   [f] and whose definition is [proc]. *)

let translate_procedure f proc =

  (* Allocate a fresh universe of pseudo-registers. *)

  let runiverse = Register.new_universe "%" in

  (* Allocate a reference that will hold the set of all
     pseudo-registers in use. *)

  let locals = ref Register.Set.empty in

  (* Define a function that allocates a fresh pseudo-register, adds it
     to the set of pseudo-registers in use, and returns it. *)

  let allocate () =
    let register = Register.fresh runiverse in
    locals := Register.Set.add register !locals;
    register
  in

  (* Create an environment that maps the formal parameters, the result
     variable, and the local variables to fresh pseudo-registers. *)

  let env, formals =
    List.fold_right (fun formal (env, formals) ->
      let register = allocate() in
      StringMap.add formal register env, register :: formals
    ) proc.UPP.formals (StringMap.empty, [])
  in

  let env, result =
    if proc.UPP.result then
      let register = allocate() in
      StringMap.add f register env, Some register
    else
      env, None
  in

  let env =
    StringSet.fold (fun local env ->
      let register = allocate() in
      StringMap.add local register env
    ) proc.UPP.locals env
  in

  (* Define a function that looks up a variable in this
     environment. *)

  let lookup x =
    try
      StringMap.find x env
    with Not_found ->
      assert false
  in

  (* Allocate a fresh universe of control flow graph labels. *)

  let luniverse = Label.new_universe f in

  (* Allocate a reference that will hold the control flow graph,
     represented as a mapping of labels to instructions. Define a
     function that adds an instruction at a fresh label to the control
     flow graph. *)

  let graph, generate = Label.Map.generator luniverse in

  (* Define a function that adds an [IGoto] instruction at a fresh
     label [label] to the control flow graph. The construction is
     recursive, in the sense that the target label of the branch
     instruction is provided by a computation that is allowed to use
     [label] itself. This allows creating cycles in the control flow
     graph.

     The definition of this function is made somewhat subtle by the
     fact that the [subgraph] function is allowed to alter the graph.
     Thus, the definition of [t] cannot be inlined into the next
     line. *)

  let loop (subgraph : Label.t -> Label.t) : Label.t =
    let exit = Label.fresh luniverse in
    let entry = subgraph exit in
    graph := Label.Map.add exit (RTL.IGoto entry) !graph;
    entry
  in

  (* Allocate a graph label that stands for the procedure's exit
     point. No instruction is associated to it. *)

  let exit = Label.fresh luniverse in

  (* Define a function that allows recognizing the exit label.
     This is used to determine which instructions are in tail
     position. *)

  let is_exit label =
    Label.equal exit label
  in

  (* We are now ready to instantiate the functor that deals with the
     translation of expressions, conditions, and instructions. The
     reason why we make this a separate functor is purely pedagogical.
     Smaller modules are easier to understand. *)

  let module I = Upp2rtlI.Make (struct
    let lookup = lookup
    let allocate = allocate
    let generate = generate
    let loop = loop
    let is_exit = is_exit
    let result = if proc.UPP.result then Some f else None
  end) in

  (* Translate the procedure's body. This yields the control flow
     graph's entry label. *)

  let entry = I.translate_instruction proc.UPP.body exit in

  (* This sums it up. *)

  {
    RTL.formals = formals;
    RTL.result = result;
    RTL.runiverse = runiverse;
    RTL.locals = !locals;
    RTL.luniverse = luniverse;
    RTL.entry = entry;
    RTL.exit = exit;
    RTL.graph = !graph
  }

(* ------------------------------------------------------------------------- *)

(* Translating programs. *)

let translate_program (p : UPP.program) : RTL.program = {
  RTL.globals = p.UPP.globals;
  RTL.defs = StringMap.mapi translate_procedure p.UPP.defs
}

