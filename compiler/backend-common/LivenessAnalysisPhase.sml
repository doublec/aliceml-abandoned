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

	fun processArgs (OneArg id, set, x) = x (set, id)
	  | processArgs (TupArgs ids, set, x) =
	    List.app (fn id => x (set, id)) ids
	  | processArgs (RecArgs labIdList, set, x) =
	    List.app (fn (_, id) => x (set, id)) labIdList

	(* Compute `Use' Sets *)

	fun del (r as ref (Orig set), Id (_, stamp, _)) =
	    if StampSet.member (set, stamp) then
		let
		    val set' = StampSet.copy set
		in
		    r := Copy set'; StampSet.delete (set', stamp)
		end
	    else ()
	  | del (ref (Copy set), Id (_, stamp, _)) =
	    StampSet.delete (set, stamp)

	fun delList (set, ids) = List.app (fn id => del (set, id)) ids

	fun ins (r as ref (Orig set), Id (_, stamp, _)) =
	    if StampSet.member (set, stamp) then ()
	    else
		let
		    val set' = StampSet.copy set
		in
		    r := Copy set'; StampSet.insert (set', stamp)
		end
	  | ins (ref (Copy set), Id (_, stamp, _)) =
	    StampSet.insert (set, stamp)

	fun insList (set, ids) = List.app (fn id => ins (set, id)) ids

	fun union (r as ref (Orig set), set') =
	    let
		val set'' = StampSet.copy set
	    in
		r := Copy set''; StampSet.union (set'', set')
	    end
	  | union (ref (Copy set), set') = StampSet.union (set, set')

	fun scanTest (LitTest _, _) = ()
	  | scanTest (ConTest (id, NONE), set) = ins (set, id)
	  | scanTest (ConTest (id1, SOME id2), set) =
	    (ins (set, id1); del (set, id2))
	  | scanTest (RefTest id, set) = del (set, id)
	  | scanTest (TupTest ids, set) = delList (set, ids)
	  | scanTest (RecTest labIdList, set) =
	    List.app (fn (_, id) => del (set, id)) labIdList
	  | scanTest (LabTest (_, id), set) = del (set, id)
	  | scanTest (VecTest ids, set) = delList (set, ids)

	fun delStm (RecDec (_, idExpList, _), set) =
	    let
		val set' = StampSet.copy set
	    in
		List.app (fn (Id (_, stamp, _), _) =>
			  StampSet.delete (set', stamp)) idExpList;
		Copy set'
	    end
	  | delStm (_, set) = Orig set

	fun scanStm (ValDec (_, id, exp, _), set) =
	    (del (set, id); scanExp (exp, set))
	  | scanStm (RecDec (_, idExpList, _), set) =
	    List.app (fn (_, exp) => scanExp (exp, set)) idExpList
	  | scanStm (EvalStm (_, exp), set) = scanExp (exp, set)
	  | scanStm (RaiseStm (_, id), set) = ins (set, id)
	  | scanStm (HandleStm (_, body1, id, body2, body3, _), set) =
	    let
		val set3 = scanBody (body3, Copy (StampSet.new ()))
		val set2 = scanBody (body2, set3)
		val set1 = scanBody (body1, set2)
	    in
		union (set, lazyValOf set1); union (set, lazyValOf set2);
		union (set, lazyValOf set3); del (set, id)
	    end
	  | scanStm (EndHandleStm (_, _), _) = ()
	  | scanStm (TestStm (_, id, test, body1, body2), set) =
	    (scanBody' (body1, set); scanTest (test, set);
	     scanBody' (body2, set); ins (set, id))
	  | scanStm (SharedStm ((_, r as ref Unknown), body, _), set) =
	    (r := LoopStart; scanBody' (body, set); r := Unknown)
	  | scanStm (SharedStm ((_, r as ref LoopStart), body, _), set) =
	    (r := LoopEnd; scanBody' (body, set))
	  | scanStm (SharedStm ((_, r as ref LoopEnd), _, _), set) = ()
	  | scanStm (SharedStm ((_, ref (Use set')), _, _), set) =
	    union (set, set')
	  | scanStm (SharedStm ((_, ref (Kill _)), _, _), _) =
	    raise Crash.Crash "LivenessAnalysisPhase.scanStm"
	  | scanStm (ReturnStm (_, exp), set) = scanExp (exp, set)
	  | scanStm (IndirectStm (_, ref bodyOpt), set) =
	    scanBody' (valOf bodyOpt, set)
	  | scanStm (ExportStm (_, exp), set) = scanExp (exp, set)
	and scanExp (LitExp (_, _), _) = ()
	  | scanExp (PrimExp (_, _), _) = ()
	  | scanExp (NewExp (_, _, _), _) = ()
	  | scanExp (VarExp (_, id), set) = ins (set, id)
	  | scanExp (ConExp (_, id, _), set) = ins (set, id)
	  | scanExp (RefExp _, _) = ()
	  | scanExp (TupExp (_, ids), set) = insList (set, ids)
	  | scanExp (RecExp (_, labIdList), set) =
	    List.app (fn (_, id) => ins (set, id)) labIdList
	  | scanExp (SelExp (_, _), _) = ()
	  | scanExp (VecExp (_, ids), set) = insList (set, ids)
	  | scanExp (FunExp (_, _, _, argsBodyList), set) =
	    List.app (fn (args, body) =>
		      (scanBody' (body, set);
		       processArgs (args, set, del))) argsBodyList
	  | scanExp (AppExp (_, id, args), set) =
	    (ins (set, id); processArgs (args, set, ins))
	  | scanExp (SelAppExp (_, _, id), set) = ins (set, id)
	  | scanExp (ConAppExp (_, id, args), set) =
	    (ins (set, id); processArgs (args, set, ins))
	  | scanExp (RefAppExp (_, args), set) = processArgs (args, set, ins)
	  | scanExp (PrimAppExp (_, _, ids), set) = insList (set, ids)
	  | scanExp (AdjExp (_, id1, id2), set) =
	    (ins (set, id1); ins (set, id2))
	and scanBody (stm::stms, initial) =
	    let
		val setRef = ref (scanBody (stms, initial))
		val _ = scanStm (stm, setRef)
		val set = lazyValOf (!setRef)
	    in
		case infoStm stm of
		    (_, r as ref (Unknown | LoopEnd)) => r := Use set
		  | (_, ref (LoopStart | Use _)) => ()
		  | (_, ref (Kill _)) =>
			raise Crash.Crash "LivenessAnalysisPhase.scanBody";
		delStm (stm, set)
	    end
	  | scanBody (nil, initial) = initial
	and scanBody' (body, set) =
	    union (set, lazyValOf (scanBody (body, Copy (StampSet.new ()))))

	(* Compute `Def' and `Kill' sets *)

	fun ins (set, Id (_, stamp, _)) = StampSet.insert (set, stamp)

	fun insList (set, ids) = List.app (fn id => ins (set, id)) ids

	fun initTest (LitTest _, _) = ()
	  | initTest (ConTest (_, NONE), _) = ()
	  | initTest (ConTest (_, SOME id), set) = ins (set, id)
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
	  | initStm (HandleStm (_, body1, id, body2, body3, _), set) =
	    let
		val set' = StampSet.copy set
	    in
		ins (set', id);
		initBody (body2, set');
		initBody (body1, StampSet.copy set);
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
	and initExp (FunExp (_, _, _, argsBodyList)) =
	    List.app (fn (args, body) =>
		      let
			  val set = StampSet.new ()
		      in
			  processArgs (args, set, ins); initBody (body, set)
		      end) argsBodyList
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

	fun annotate (_, _, body) =
	    (scanBody (body, Copy (StampSet.new ()));
	     initBody (body, StampSet.new ()))
    end
