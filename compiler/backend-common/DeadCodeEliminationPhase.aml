(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure DeadCodeEliminationPhase :> DEAD_CODE_ELIMINATION_PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = FlatGrammar

	open I

	fun killSet (stm::_) =
	    (case infoStm stm of
		 {liveness = ref (Kill set), ...} => set
	       | _ => raise Crash.Crash "DeadCodeEliminationPhase.killSet 1")
	  | killSet nil =
	    raise Crash.Crash "DeadCodeEliminationPhase.killSet 2"

	fun killId (Id (_, stamp, _), set) =
	    if StampSet.member (set, stamp) then
		(StampSet.delete (set, stamp); true)
	    else false

	fun killIdOpt (idOpt as SOME id, set) =
	    if killId (id, set) then NONE else idOpt
	  | killIdOpt (NONE, set) = NONE

	fun await (info, id) = PrimAppExp (info, "Future.await", [id])

	fun liveBody (ValDec (info, id, exp)::rest) =
	    let
		val set = killSet rest
	    in
		if killId (id, set) then liveBody (EvalStm (info, exp)::rest)
		else ValDec (info, id, liveExp exp)::liveBody rest
	    end
	  | liveBody (RecDec (info, idExpList)::rest) =
	    RecDec (info, List.map (fn (id, exp) =>
				    (id, liveExp exp)) idExpList)::
	    liveBody rest
	  | liveBody ((stm as RefAppDec (info, id1, id2))::rest) =
	    (if killId (id1, killSet rest) then await (info, id2) else stm)::
	    liveBody rest
	  | liveBody (TupDec (info, idOpts, id)::rest) =
	    let
		val set = killSet rest
		val idOpts =
		    List.map (fn idOpt => killIdOpt (set, idOpt)) idOpts
	    in
		(if List.all Option.isNone isOpts then await (info, id)
		 else TupDec (info, idOpts, id))::liveBody rest
	    end
	  | liveBody (RowDec (info, labelIdOptList, id)::rest) =
	    let
		val set = killSet rest
		val labelIdOptList =
		    List.map (fn (label, idOpt) =>
			      (label, killIdOpt (idOpt, set))) labelIdOptList
	    in
		(if List.all (fn (_, id) => Option.isNone id) isOpts then
		     await (info, id)
		 else RowDec (info, labelIdOptList, id))::liveBody rest
	    end
	  | liveBody (EvalStm (info, exp)::rest) =
	    (case deadExp exp of
		 SOME exp => EvalStm (info, exp)::liveBody rest
	       | NONE => liveBody rest)
	  | liveBody (body as [RaiseStm (_, _)]) = body
	  | liveBody (body as [ReraiseStm (_, _)]) = body
	  | liveBody [HandleStm (info, body1, idOpt, body2, body3, stamp)] =
	    let
		val body1 = liveBody body1
		val body2 = liveBody body2
		val body3 = liveBody body3
		val idOpt = killIdOpt (idOpt, killSet body2)
	    in
		case body1 of
		    _::_ =>
			[HandleStm (info, body1, idOpt, body2, body3, stamp)]
		  | nil => body3
	    end
	  | liveBody (body as [EndHandleStm (_, _)]) = body
	  | liveBody [TestStm (info, id, testBodyList, body)] =


	datatype test =
	    LitTest of lit
	  | TagTest of label * int
	  | TagAppTest of label * int * id args
	  | ConTest of id
	  | ConAppTest of id * id args
	  | StaticConTest of stamp
	  | StaticConAppTest of stamp * id args
	  | VecTest of id list

	datatype stm =
	    ...
	  | TestStm of stm_info * id * (test * body) list * body
	  | SharedStm of stm_info * body * stamp   (* used at least twice *)
	  | ReturnStm of stm_info * exp
	  | IndirectStm of stm_info * body option ref
	  | ExportStm of stm_info * exp
	and exp =
	    LitExp of exp_info * lit
	  | PrimExp of exp_info * string
	  | NewExp of exp_info * conArity
	  | VarExp of exp_info * id
	  | TagExp of exp_info * label * int * conArity
	  | ConExp of exp_info * id * conArity
	  | StaticConExp of exp_info * stamp * conArity
	  | RefExp of exp_info
	  | TupExp of exp_info * id list
	  | RowExp of exp_info * (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of exp_info * label * int
	  | VecExp of exp_info * id list
	  | FunExp of exp_info * stamp * funFlag list * id args * body
	  | PrimAppExp of exp_info * string * id list
	  | VarAppExp of exp_info * id * id args
	  | TagAppExp of exp_info * label * int * id args
	  | ConAppExp of exp_info * id * id args
	  | StaticConAppExp of exp_info * stamp * id args
	  | RefAppExp of exp_info * id
	  | SelAppExp of exp_info * label * int * id
	  | FunAppExp of exp_info * id * stamp * id args
	withtype body = stm list


	fun translate () (_, component as (imports, (body, sign))) =
	    (imports, (liveBody body, sign))
    end
