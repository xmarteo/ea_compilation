open MIPSOps

(* ------------------------------------------------------------------------- *)

(* The functor [Visit] implements the core of the translation. *)

module Visit (S : sig

  (* [fetch l] is the instruction found at label [l] in the source
     program. *)

  val fetch: Label.t -> LTL.instruction

  (* [translate_instruction i] translates the [LTL] instruction [i] to
     a [LIN] instruction. [LTL] instructions that have one explicit
     successor are turned into [LIN] instructions with an implicit
     successor. [LTL] instructions that have two explicit successors
     are turned into [LIN] instructions where the first successor is
     explicit and the second successor is implicit. *)

  val translate_instruction: LTL.instruction -> LIN.instruction

  (* [generate i] generates instruction [i]. Instructions are
     generated sequentially. *)

  val generate: LIN.instruction -> unit

  (* [require l] records the fact that the label [l] should explicitly
     exist in the [LIN] program. It must be used whenever a [LIN]
     branch instruction is issued. *)

  val require: Label.t -> unit

  (* [mark l] marks the label [l]. [marked l] tells whether [l] is
     marked. *)

  val mark: Label.t -> unit
  val marked: Label.t -> bool

end) = struct

  open S

  let rec visit l =

    if marked l then begin

      (* Label [l] has been visited before. This implies that an [ILabel l]
	 instruction has been issued already. We must now generate an
	 [IGoto] instruction that transfers control to this place. Because
	 [l] is the target of a branch instruction, we require it to exist
	 in the [LIN] code. *)

      require l;
      generate (LIN.IGoto l)

    end
    else begin

      (* Label [l] has never been visited before. First, record that it
	 now has been visited, so as to avoid looping. *)

      mark l;

      (* Then, generate an [ILabel l] instruction. This instruction
	 will be useless if [l] turns out not to be the target of a
	 branch: this is taken care of later. *)

      generate (LIN.ILabel l);

      (* Fetch the instruction found at label [l] in the source program. *)

      let instruction = fetch l in

      (* As an optional optimization, check if this is a conditional branch
	 whose implicit successor has been visited before and whose explicit
	 successor has not. In that case, if we did nothing special, we would
	 produce a conditional branch immediately followed with an
	 unconditional one, like this:

		bgtz  $t1, find24
		j     find42
		find24:
                ...

	 This can be avoided simply by reversing the condition:

                blez  $t1, find42
                ...

	 *)

      let instruction =
	match instruction with

	| LTL.IUnBranch (cond, r, l1, l2)
	  when marked l2 && not (marked l1) ->
	    let cond =
	      match cond with
	      | UConGez ->
		  UConLtz
	      | UConGtz ->
		  UConLez
	      | UConLez ->
		  UConGtz
	      | UConLtz ->
		  UConGez
	    in
	    LTL.IUnBranch (cond, r, l2, l1)

	| LTL.IBinBranch (cond, r1, r2, l1, l2)
	  when marked l2 && not (marked l1) ->
	    let cond =
	      match cond with
	      | ConEq ->
		  ConNe
	      | ConNe ->
		  ConEq
	    in
	    LTL.IBinBranch (cond, r1, r2, l2, l1)

	| _ ->
	    instruction
      in

      (* Translate the instruction. *)

      generate (translate_instruction instruction);

      (* Note that [translate_instruction] never produces an [IGoto]
	 instruction. As a result, the code above never generates an [ILabel]
	 instruction immediately followed with an [IGoto] instruction. This
	 proves that we never generate a (conditional or unconditional) branch
	 towards an [IGoto] instruction. *)

      (* There now remains to visit the instruction's successors. *)

      match instruction with

	(* Sequential instructions. There is only one successor, with implicit
	   fallthrough. *)

      | LTL.INewFrame l
      | LTL.IDeleteFrame l
      | LTL.IGetStack (_, _, l)
      | LTL.ISetStack (_, _, l)
      | LTL.IConst (_, _, l)
      | LTL.IUnOp (_, _, _, l)
      | LTL.IBinOp (_, _, _, _, l)
      | LTL.ICall (_, l)
      | LTL.ILoad (_, _, _, l)
      | LTL.IStore (_, _, _, l)
      | LTL.IGoto l ->

	  visit l

	(* Conditional branch instructions. The label that is reached by
	   falling through in [LIN] is [l2], which means that it must be
	   translated first, so that its instructions are contiguous with the
	   [LIN] branch instruction. The label [l1], on the other hand,
	   becomes the target of a jump, so it is required to exist in the
	   [LIN] code.

	   Code for [l1] is generated, if necessary, after we are done dealing
	   with [l2]. If [l1] has already been visited at this point, no code
	   needs be produced, so the second call to visit is made only if [l1]
	   has not been visited yet. *)

      | LTL.IUnBranch (_, _, l1, l2)
      | LTL.IBinBranch (_, _, _, l1, l2) ->

	  visit l2;
	  require l1;
	  if not (marked l1) then
	    visit l1

	(* Instructions without a successor. *)

	(* We would prefer to duplicate, rather than share, these
	   instructions. Indeed, it is inefficient to generate a jump towards
	   one of these instructions. Unfortunately, it is not easy to achieve
	   this, for two reasons. First, IDeleteFrame is in the way. Second,
	   and worse, we must not generate duplicate labels. Maybe I will find
	   a fix in the future. *)

      | LTL.IReturn
      | LTL.ITailCall _ ->
	  
	  ()

    end

end

