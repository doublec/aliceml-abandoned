(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure PartialEvaluationPhase :> PARTIAL_EVALUATION_PHASE =
    struct
	(* After value propagation, identifier references are "flat",
	 * i.e., an identifier may still be referenced after it has
	 * left its scope (unless this was a function body).  However,
	 * an identifier will never be referenced before its definition.
	 *)

	structure I = SimplifiedGrammar
	structure O = SimplifiedGrammar

	open I

	(* Value Representation *)

	datatype arity =
	    TupArity of int
	  | RecArity of string list
	type inline = id * exp

	datatype valDesc =
	    Lit of lit
	  | Con of id * bool   (* has args *)
	  | ConVal of id * valRep option
	  | Tup of valRep option list
	  | Rec of (string * valRep option) list
	    (* sorted, all labels distinct, no tuple *)
	  | Fun of arity list * inline option
	  | Sel of lab
	  | Builtin of string
	  | Top
	withtype valRep = id * valDesc

	fun getValDesc NONE = Top
	  | getValDesc (SOME (_, valDesc)) = valDesc

	(* State *)

	structure StampSymtable = MakeHashScopedImpMap(Stamp)

	type state = {env: valDesc StampSymtable.t, isToplevel: bool ref}

	fun newState () =
	    {env = StampSymtable.new (), isToplevel = ref true}
	    (*--** enter toplevel environment *)

	fun enter (state: state, Id (_, stamp, _), valDesc) =
	    StampSymtable.insert (#env state, stamp, valDesc)

	fun lookup (state: state, Id (_, stamp, _)) =
	    valOf (StampSymtable.lookup (#env state, stamp))

	fun setToplevel (state: state, isToplevel) =
	    #isToplevel state := isToplevel

	fun isToplevel (state: state) = !(#isToplevel state)

	(* Value propagation proper *)

	fun idToVarExp id =
	    let
		val coord = IntermediateGrammar.infoId id
	    in
		VarExp (coord, ShortId (coord, id))
	    end

	fun evalLongId (longid as ShortId (_, id), state) =
	    (longid, SOME (id, lookup (state, id)))
	  | evalLongId (longid as LongId (coord, longid',
					  lab as Lab (_, s)), state) =
	    (case evalLongId (longid', state) of
		 (longid'', SOME (_, Rec fields)) =>
		     (case List.find (fn (s', _) => s = s') fields of
			  SOME (_, valRepOpt as SOME (id, _)) =>
			      (ShortId (coord, id), valRepOpt)
			| SOME (_, NONE) =>
			      (LongId (coord, longid'', lab), NONE)
			| NONE =>   (*--** emit warning; non-existent member *)
			      (LongId (coord, longid'', lab), NONE))
	       | (longid'', _) =>   (*--** emit warning if it's not Top *)
		     (LongId (coord, longid'', lab), NONE))

	fun evalDec (OneDec (coord, id, exp), state) =
	    let
		val (exp', valDesc) = evalExp (exp, state)
	    in
		enter (state, id, valDesc);
		OneDec (coord, id, exp')
	    end
	  | evalDec (ValDec (coord, ids, exp), state) =
	    let
		(*--** val _ = pushDec (state, ids) *)
		val (exp', _) = evalExp (exp, state)
		(*--** val _ = popDec (state, ids) *)
	    in
		ValDec (coord, ids, exp')
	    end
	  | evalDec (dec as RecDec (_, idExpList), state) =
	    dec   (*--** *)
	  | evalDec (dec as ConDec (_, id, hasArgs), state) =
	    let
		val valDesc =
		    if isToplevel state then Con (id, hasArgs) else Top
	    in
		enter (state, id, valDesc);
		dec
	    end
	and evalExp (exp as LitExp (_, lit), _) = (exp, Lit lit)
	  | evalExp (VarExp (coord, longid), state) =
	    let
		val (longid', valRepOpt) = evalLongId (longid, state)
	    in
		(*--** replace variable by literal if bound to one? *)
		(VarExp (coord, longid'), getValDesc valRepOpt)
	    end
	  | evalExp (ConExp (coord, longid, NONE), state) =
	    let
		val (longid', valRepOpt) = evalLongId (longid, state)
	    in
		(ConExp (coord, longid', NONE), getValDesc valRepOpt)
	    end
	  | evalExp (ConExp (coord, longid1, SOME longid2), state) =
	    let
		val (longid1', valRepOpt1) = evalLongId (longid1, state)
		val (longid2', valRepOpt2) = evalLongId (longid2, state)
	    in
		(ConExp (coord, longid1', SOME longid2'),
		 case valRepOpt1 of
		     SOME (_, Con (id, true)) =>
			 (*--** use (longid2', Top) instead of valRep2? *)
			 ConVal (id, valRepOpt2)
		   | _ => Top)
	    end
	  | evalExp (exp as TupExp (coord, longids), state) =
	    let
		val (longids', valRepOpts) =
		    ListPair.unzip
		    (List.map (fn longid =>
			       evalLongId (longid, state)) longids)
	    in
		(TupExp (coord, longids'), Tup valRepOpts)
	    end
	  | evalExp (exp as RecExp (coord, labLongIdList), state) =
	    let
		val (labLongIdList', labValRepOptList) =
		    ListPair.unzip
		    (List.map (fn (lab as Lab (_, s), longid) =>
			       let
				   val (longid', valRepOpt) =
				       evalLongId (longid, state)
			       in
				   ((lab, longid'), (s, valRepOpt))
			       end) labLongIdList)
	    in
		(RecExp (coord, labLongIdList'), Rec labValRepOptList)
	    end
	  | evalExp (exp as SelExp (_, lab, NONE), state) =
	    (exp, Sel lab)
	  | evalExp (SelExp (coord, lab as Lab (_, s), SOME exp), state) =
	    let
		val (exp', valDesc) = evalExp (exp, state)
	    in
		case valDesc of
		    Rec fields =>
			(case List.find (fn (s', _) => s = s') fields of
			     SOME (_, SOME (id, valDesc)) =>
				 (idToVarExp id, valDesc)
			   | SOME (_, NONE) =>
				 (SelExp (coord, lab, SOME exp'), Top)
			   | NONE =>   (*--** emit a warning *)
				 (SelExp (coord, lab, SOME exp'), Top))
		  | _ => (SelExp (coord, lab, SOME exp'), Top)
	    end
	  | evalExp (exp as FunExp (coord, _, argExpList), state) =
	    let
		val immediate =
		    List.foldr (fn ((args, exp), rest) =>
				case args of
				    OneArg _ => rest
				  | TupArgs ids => TupArity (length ids)::rest
				  | RecArgs stringIdList =>
					RecArity (List.map (fn (s, _) => s)
						  stringIdList)::rest)
		    nil argExpList
		val inlineOpt = NONE   (*--** *)
	    in
		(*--** compute print name *)
		(*--** descend *)
		(exp, Fun (immediate, inlineOpt))
	    end
(*
	  | evalExp (AppExp (coord, longid, exp, isTailRef), state) =
	    let
		val (longid', valRepOpt) = evalLongId (longid, state)
	    in
		case valRepOpt of
		    SOME (Fun (immediate, inlineOpt)) =>   (*--** *)
		  | SOME (Sel lab) =>
			evalExp (SelExp (coord, lab, SOME exp), state)
		  | SOME (Builtin s) =>   (*--** *)
		  | NONE =>
			let
			    val (exp', _) = evalExp (exp, state)
			in
			    AppExp (coord, longid', exp', isTailRef)
			end
	    end
*)
	  | evalExp (exp, state) = (exp, Top)   (*--** *)

(*--**
	datatype exp =
	    ...
	  | AppExp of coord * longid * exp * bool ref
	  | AdjExp of coord * exp * exp
	  | WhileExp of coord * exp * exp
	  | SeqExp of coord * exp list
	  | TestExp of coord * longid * test * exp * exp
	  | RaiseExp of coord * exp
	  | HandleExp of coord * exp * id * exp
	  | LetExp of coord * dec list * exp
	  | SharedExp of coord * exp * shared
	  | DecExp of coord * id list
*)


	fun main (decs, ids) =
	    let
		val state = newState ()
	    in
		(List.map (fn dec => evalDec (dec, state)) decs, ids)
	    end
    end
