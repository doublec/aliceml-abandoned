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

(*
 * The `Use' set of a statement is the set of stamps that
 * have already been initialized when the statement is reached
 * and that are still going to be referenced within or after it.
 *
 * Dead code elimination for defining occurrences without using occurrences:
 *    stm = ValDec (... stamp ...): stamp \in Kill(Cont(stm))
 *    stm = RecDec (... stamp ...): stamp \in Kill(Cont(stm))   (*--** check *)
 *    stm = HandleStm (... stamp ... catchBody ...): stamp \in Kill(catchBody)
 *    stm = TestStm (... stamp ... thenBody ...): stamp \in Kill(thenBody)
 *)

structure LivenessAnalysisPhase :> LIVENESS_ANALYSIS_PHASE =
    struct
	structure I = ImperativeGrammar
	open I

	datatype 'a lazyCopy =
	    Orig of 'a
	  | Copy of 'a

	fun lazyValOf (Orig x) = x
	  | lazyValOf (Copy x) = x

	fun processArgs (OneArg id, lset, x) = x (lset, id)
	  | processArgs (TupArgs ids, lset, x) =
	    List.foldl (fn (id, lset) => x (lset, id)) lset ids
	  | processArgs (RecArgs labIdList, lset, x) =
	    List.foldl (fn ((_, id), lset) => x (lset, id)) lset labIdList

	(* Compute `Use' Sets *)

	fun del (lset as (Orig set), Id (_, stamp, _)) =
	    if StampSet.member (set, stamp) then
		let
		    val set' = StampSet.copy set
		in
		    StampSet.delete (set', stamp);
		    Copy set'
		end
	    else lset
	  | del (lset as (Copy set), Id (_, stamp, _)) =
	    (StampSet.delete (set, stamp); lset)

	fun delList (lset, ids) =
	    List.foldl (fn (id, lset) => del (lset, id)) lset ids

	fun ins (lset as (Orig set), Id (_, stamp, _)) =
	    if StampSet.member (set, stamp) then lset
	    else
		let
		    val set' = StampSet.copy set
		in
		    StampSet.insert (set', stamp);
		    Copy set'
		end
	  | ins (lset as (Copy set), Id (_, stamp, _)) =
	    (StampSet.insert (set, stamp); lset)

	fun insList (lset, ids) =
	    List.foldl (fn (id, lset) => ins (lset, id)) lset ids

	fun union (Orig set, set') =
	    let
		val set'' = StampSet.copy set
	    in
		StampSet.union (set'', set');
		Copy set''
	    end
	  | union (lset as (Copy set), set') =
	    (StampSet.union (set, set'); lset)

	fun scanTest (LitTest _, lset) = lset
	  | scanTest (ConTest (id, NONE, _), lset) = ins (lset, id)
	  | scanTest (ConTest (id1, SOME id2, _), lset) =
	    del (ins (lset, id1), id2)
	  | scanTest (RefTest id, lset) = del (lset, id)
	  | scanTest (TupTest ids, lset) = delList (lset, ids)
	  | scanTest (RecTest labIdList, lset) =
	    List.foldl (fn ((_, id), lset) => del (lset, id)) lset labIdList
	  | scanTest (LabTest (_, id), lset) = del (lset, id)
	  | scanTest (VecTest ids, lset) = delList (lset, ids)

	fun setInfo ((_, r as ref (Unknown | LoopStart | LoopEnd)), set) =
	    r := Use set
	  | setInfo ((_, ref (Use _)), _) = ()
	  | setInfo ((_, ref (Kill _)), _) =
	    raise Crash.Crash "LivenessAnalysisPhase.setInfo"

	(* Annotate the `Use' set at each statement *)

	fun scanBody (ValDec (i, id, exp, _)::stms, initial) =
	    let
		val lset = scanBody (stms, initial)
		val set = lazyValOf (scanExp (exp, del (lset, id)))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody (RecDec (i, idExpList, _)::stms, initial) =
	    let
		val lset = scanBody (stms, initial)
		val lset' =
		    List.foldl (fn ((_, exp), lset) => scanExp (exp, lset))
		    lset idExpList
		val set = lazyValOf lset'
		val _ = setInfo (i, set)
		val set' = StampSet.copy set
	    in
		List.app (fn (Id (_, stamp, _), _) =>
			  StampSet.delete (set', stamp)) idExpList;
		Copy set'
	    end
	  | scanBody (EvalStm (i, exp)::stms, initial) =
	    let
		val lset = scanBody (stms, initial)
		val set = lazyValOf (scanExp (exp, lset))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([RaiseStm (i, Id (_, stamp, _))], _) =
	    let
		val set = StampSet.new ()
		val _ = StampSet.insert (set, stamp)
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([ReraiseStm (i, Id (_, stamp, _))], _) =
	    let
		val set = StampSet.new ()
		val _ = StampSet.insert (set, stamp)
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([HandleStm (i, body1, id, body2, body3, _)], initial) =
	    let
		val lset3 = scanBody (body3, initial)
		val lset2 = scanBody (body2, lset3)
		val lset1 = scanBody (body1, lset2)
		val set = lazyValOf (del (lset1, id))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([EndHandleStm (i, _)], initial) =
	    let
		val set = lazyValOf initial
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([TestStm (i, id, test, body1, body2)], initial) =
	    let
		val initial' = Orig (lazyValOf initial)
		val lset1 = scanTest (test, scanBody (body1, initial'))
		val lset2 = scanTest (test, scanBody (body2, initial'))
		val lset1' = union (lset1, lazyValOf (ins (lset2, id)))
		val set = lazyValOf lset1'
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([SharedStm (i as (_, r as ref Unknown), body, _)],
		      initial) =
	    let
		val _ = r := LoopStart
		val set = lazyValOf (scanBody (body, initial))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([SharedStm (i as (_, r as ref LoopStart), body, _)],
		      initial) =
	    (r := LoopEnd; scanBody (body, initial))
	  | scanBody ([SharedStm ((_, r as ref LoopEnd), _, _)],
		      initial) = Copy (StampSet.new ())   (*--** or initial? *)
	  | scanBody ([SharedStm ((_, ref (Use set')), _, _)], _) = Orig set'
	  | scanBody ([SharedStm ((_, ref (Kill _)), _, _)], _) =
	    raise Crash.Crash "LivenessAnalysisPhase.scanStm 1"
	  | scanBody ([ReturnStm (i, exp)], _) =
	    let
		val set = lazyValOf (scanExp (exp, Copy (StampSet.new ())))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([IndirectStm (i, ref bodyOpt)], initial) =
	    let
		val set = lazyValOf (scanBody (valOf bodyOpt, initial))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([ExportStm (i, exp)], _) =
	    let
		val set = lazyValOf (scanExp (exp, Copy (StampSet.new ())))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody (nil, initial) = initial
	  | scanBody (_, _) =
	    raise Crash.Crash "LivenessAnalysisPhase.scanStm 2"
	and scanExp (LitExp (_, _), lset) = lset
	  | scanExp (PrimExp (_, _), lset) = lset
	  | scanExp (NewExp (_, _, _), lset) = lset
	  | scanExp (VarExp (_, id), lset) = ins (lset, id)
	  | scanExp (ConExp (_, id, _), lset) = ins (lset, id)
	  | scanExp (RefExp _, lset) = lset
	  | scanExp (TupExp (_, ids), lset) = insList (lset, ids)
	  | scanExp (RecExp (_, labIdList), lset) =
	    List.foldl (fn ((_, id), lset) => ins (lset, id)) lset labIdList
	  | scanExp (SelExp (_, _), lset) = lset
	  | scanExp (VecExp (_, ids), lset) = insList (lset, ids)
	  | scanExp (FunExp (_, _, _, args, body), lset) =
	    let
		val set =
		    lazyValOf (scanBody (body, Copy (StampSet.new ())))
	    in
		processArgs (args, union (lset, set), del)
	    end
	  | scanExp (AppExp (_, id, args), lset) =
	    processArgs (args, ins (lset, id), ins)
	  | scanExp (SelAppExp (_, _, id), lset) = ins (lset, id)
	  | scanExp (ConAppExp (_, id, args, _), lset) =
	    processArgs (args, ins (lset, id), ins)
	  | scanExp (RefAppExp (_, args), lset) = processArgs (args, lset, ins)
	  | scanExp (PrimAppExp (_, _, ids), lset) = insList (lset, ids)
	  | scanExp (AdjExp (_, id1, id2), lset) = ins (ins (lset, id1), id2)

	(* Compute `Def' and `Kill' sets *)

	fun processArgs (OneArg id, set, x) = x (set, id)
	  | processArgs (TupArgs ids, set, x) =
	    List.app (fn id => x (set, id)) ids
	  | processArgs (RecArgs labIdList, set, x) =
	    List.app (fn (_, id) => x (set, id)) labIdList

	fun ins (set, Id (_, stamp, _)) = StampSet.insert (set, stamp)

	fun insList (set, ids) = List.app (fn id => ins (set, id)) ids

	fun initTest (LitTest _, _) = ()
	  | initTest (ConTest (_, NONE, _), _) = ()
	  | initTest (ConTest (_, SOME id, _), set) = ins (set, id)
	  | initTest (RefTest id, set) = ins (set, id)
	  | initTest (TupTest ids, set) = insList (set, ids)
	  | initTest (RecTest labIdList, set) =
	    List.app (fn (_, id) => ins (set, id)) labIdList
	  | initTest (LabTest (_, id), set) = ins (set, id)
	  | initTest (VecTest ids, set) = insList (set, ids)

	fun initStm (ValDec (_, id, exp, _), set) =
	    (ins (set, id); initExp exp)
	  | initStm (RecDec (_, idExpList, _), set) =
	    List.app (fn (id, exp) => (ins (set, id); initExp exp)) idExpList
	  | initStm (EvalStm (_, exp), _) = initExp exp
	  | initStm (RaiseStm (_, _), _) = ()
	  | initStm (ReraiseStm (_, _), _) = ()
	  | initStm (HandleStm (_, body1, id, body2, body3, _), set) =
	    let
		val set' = StampSet.copy set
	    in
		ins (set', id);
		initBody (body1, StampSet.copy set);
		initBody (body2, set');
		initBody (body3, set)
	    end
	  | initStm (EndHandleStm (_, _), _) = ()
	  | initStm (TestStm (_, _, test, body1, body2), set) =
	    let
		val set' = StampSet.copy set
	    in
		initTest (test, set'); initBody (body1, set');
		initBody (body2, set)
	    end
	  | initStm (SharedStm ((_, ref (Kill _)), _, _), _) = ()
	  | initStm (SharedStm (_, body, _), set) = initBody (body, set)
	  | initStm (ReturnStm (_, exp), _) = initExp exp
	  | initStm (IndirectStm (_, ref bodyOpt), set) =
	    initBody (valOf bodyOpt, set)
	  | initStm (ExportStm (_, _), _) = ()
	and initExp (FunExp (_, _, _, args, body)) =
	    let
		val set = StampSet.new ()
	    in
		processArgs (args, set, ins); initBody (body, set)
	    end
	  | initExp _ = ()
	and initBody (stm::stms, defSet) =
	    (case infoStm stm of
		 (_, ref (Unknown | LoopStart | LoopEnd)) =>
		     raise Crash.Crash "LivenessAnalysisPhase.initBody"
	       | (_, r as ref (Use useSet)) =>
		     let
			 val killSet = StampSet.new ()
		     in
			 StampSet.app
			 (fn stamp =>
			  if StampSet.member (useSet, stamp) then ()
			  else StampSet.insert (killSet, stamp)) defSet;
			 StampSet.app
			 (fn stamp => StampSet.delete (defSet, stamp)) killSet;
			 initStm (stm, defSet);
			 r := Kill killSet;
			 initBody (stms, defSet)
		     end
	       | (_, ref (Kill _)) => ())
	  | initBody (nil, _) = ()

	fun annotate (_, (body, _)) =
	    (scanBody (body, Copy (StampSet.new ()));
	     initBody (body, StampSet.new ()))
    end
