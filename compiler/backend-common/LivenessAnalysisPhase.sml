(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
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
 * The `Kill' set of a statement is the set of stamps that -
 * starting from this statement - will no longer be referenced.
 *)

structure LivenessAnalysisPhase :> LIVENESS_ANALYSIS_PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = FlatGrammar

	open I

	datatype 'a lazyCopy =
	    Orig of 'a
	  | Copy of 'a

	fun lazyValOf (Orig x) = x
	  | lazyValOf (Copy x) = x

	fun processArgs (OneArg id, lset, x) = x (lset, id)
	  | processArgs (TupArgs ids, lset, x) =
	    List.foldl (fn (id, lset) => x (lset, id)) lset ids
	  | processArgs (ProdArgs labIdList, lset, x) =
	    List.foldl (fn ((_, id), lset) => x (lset, id)) lset labIdList

	(* Compute `Use' Sets *)

	fun delDef (lset as (Orig set), IdDef (Id (_, stamp, _))) =
	    if StampSet.member (set, stamp) then
		let
		    val set' = StampSet.clone set
		in
		    StampSet.delete (set', stamp);
		    Copy set'
		end
	    else lset
	  | delDef (lset as (Copy set), IdDef (Id (_, stamp, _))) =
	    (StampSet.delete (set, stamp); lset)
	  | delDef (lset, Wildcard) = lset

	fun delDefList (lset, idDefs) =
	    List.foldl (fn (idDef, lset) => delDef (lset, idDef)) lset idDefs

	fun ins (lset as (Orig set), Id (_, stamp, _)) =
	    if StampSet.member (set, stamp) then lset
	    else
		let
		    val set' = StampSet.clone set
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
		val set'' = StampSet.clone set
	    in
		StampSet.union (set'', set');
		Copy set''
	    end
	  | union (lset as (Copy set), set') =
	    (StampSet.union (set, set'); lset)

	fun setInfo ({liveness = r as ref (Unknown | LoopStart | LoopEnd),
		      ...}: stm_info, set) =
	    r := Use set
	  | setInfo ({liveness = ref (Use _), ...}, _) = ()
	  | setInfo ({liveness = ref (Kill _), ...}, _) =
	    raise Crash.Crash "LivenessAnalysisPhase.setInfo"

	(* Annotate the `Use' set at each statement *)

	fun scanBody (ValDec (i, idDef, exp)::stms, initial) =
	    let
		val lset = scanBody (stms, initial)
		val set = lazyValOf (scanExp (exp, delDef (lset, idDef)))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody (RecDec (i, idDefExpList)::stms, initial) =
	    let
		val lset = scanBody (stms, initial)
		val lset' =
		    List.foldl (fn ((_, exp), lset) => scanExp (exp, lset))
		    lset idDefExpList
		val set = lazyValOf lset'
	    in
		setInfo (i, set);
		List.foldl (fn ((idDef, _), lset) => delDef (lset, idDef))
		(Copy (StampSet.clone set)) idDefExpList
	    end
	  | scanBody (RefAppDec (i, idDef, id)::stms, initial) =
	    let
		val lset = scanBody (stms, initial)
		val set = lazyValOf (ins (delDef (lset, idDef), id))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody (TupDec (i, idDefs, id)::stms, initial) =
	    let
		val lset = scanBody (stms, initial)
		val lset' =
		    List.foldr (fn (idDef, lset) => delDef (lset, idDef))
		    lset idDefs
		val set = lazyValOf (ins (lset', id))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody (ProdDec (i, labelIdDefList, id)::stms, initial) =
	    let
		val lset = scanBody (stms, initial)
		val lset' =
		    List.foldr (fn ((_, idDef), lset) => delDef (lset, idDef))
		    lset labelIdDefList
		val set = lazyValOf (ins (lset', id))
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
	  | scanBody ([HandleStm (i, body1, idDef, body2, body3, _)],
		      initial) =
	    let
		val lset3 = scanBody (body3, initial)
		val lset2 = scanBody (body2, lset3)
		val lset1 = scanBody (body1, union (lset2, lazyValOf lset3))
		val set = lazyValOf (delDef (lset1, idDef))
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
	  | scanBody ([TestStm (i, id, tests, body)], initial) =
	    let
		val initial' = Orig (lazyValOf initial)
		val lset1 = scanTests (tests, initial')
		val lset2 = scanBody (body, initial')
		val lset1' = ins (union (lset1, lazyValOf lset2), id)
		val set = lazyValOf lset1'
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([SharedStm (i as {liveness = r as ref Unknown, ...},
				  body, _)], initial) =
	    let
		val _ = r := LoopStart
		val set = lazyValOf (scanBody (body, initial))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody ([SharedStm (i as {liveness = r as ref LoopStart, ...},
				  body, _)], initial) =
	    (r := LoopEnd; scanBody (body, initial))
	  | scanBody ([SharedStm ({liveness = r as ref LoopEnd, ...},
				  _, _)], initial) =
	    Copy (StampSet.new ())   (*--** or initial? *)
	  | scanBody ([SharedStm ({liveness = ref (Use set'), ...},
				  _, _)], _) = Orig set'
	  | scanBody ([SharedStm ({liveness = ref (Kill _), ...}, _, _)], _) =
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
	and scanTests (LitTests litBodyList, initial) =
	    List.foldl (fn ((_, body), lset) =>
			union (lset, lazyValOf (scanBody (body, initial))))
	    initial litBodyList
	  | scanTests (TagTests tagBodyList, initial) =
	    List.foldl (fn ((_, _, conArgs, body), lset) =>
			let
			    val lset' = scanBody (body, initial)
			    val lset'' =
				case conArgs of
				    SOME args =>
					processArgs (args, lset', delDef)
				  | NONE => lset'
			in
			    union (lset, lazyValOf lset'')
			end) initial tagBodyList
	  | scanTests (ConTests conBodyList, initial) =
	    List.foldl (fn ((_, conArgs, body), lset) =>
			let
			    val lset' = scanBody (body, initial)
			    val lset'' =
				case conArgs of
				    SOME args =>
					processArgs (args, lset', delDef)
				  | NONE => lset'
			in
			    union (lset, lazyValOf lset'')
			end) initial conBodyList
	  | scanTests (VecTests vecBodyList, initial) =
	    List.foldl (fn ((idDefs, body), lset) =>
			let
			    val lset' = scanBody (body, initial)
			    val lset'' = delDefList (lset, idDefs)
			in
			    union (lset, lazyValOf lset'')
			end) initial vecBodyList
	and scanExp (LitExp (_, _), lset) = lset
	  | scanExp (PrimExp (_, _), lset) = lset
	  | scanExp (NewExp (_, _), lset) = lset
	  | scanExp (VarExp (_, id), lset) = ins (lset, id)
	  | scanExp (TagExp (_, _, _, _), lset) = lset
	  | scanExp (ConExp (_, Con id, _), lset) = ins (lset, id)
	  | scanExp (ConExp (_, StaticCon _, _), lset) = lset
	  | scanExp (RefExp _, lset) = lset
	  | scanExp (TupExp (_, ids), lset) = insList (lset, ids)
	  | scanExp (ProdExp (_, labIdList), lset) =
	    List.foldl (fn ((_, id), lset) => ins (lset, id)) lset labIdList
	  | scanExp (SelExp (_, _, _), lset) = lset
	  | scanExp (VecExp (_, ids), lset) = insList (lset, ids)
	  | scanExp (FunExp (_, _, _, args, body), lset) =
	    let
		val set = lazyValOf (scanBody (body, Copy (StampSet.new ())))
	    in
		processArgs (args, union (lset, set), delDef)
	    end
	  | scanExp (PrimAppExp (_, _, ids), lset) = insList (lset, ids)
	  | scanExp (VarAppExp (_, id, args), lset) =
	    processArgs (args, ins (lset, id), ins)
	  | scanExp (TagAppExp (_, _, _, args), lset) =
	    processArgs (args, lset, ins)
	  | scanExp (ConAppExp (_, Con id, args), lset) =
	    processArgs (args, ins (lset, id), ins)
	  | scanExp (ConAppExp (_, StaticCon _, args), lset) =
	    processArgs (args, lset, ins)
	  | scanExp (RefAppExp (_, id), lset) = ins (lset, id)
	  | scanExp (SelAppExp (_, _, _, id), lset) = ins (lset, id)
	  | scanExp (FunAppExp (_, id, _, args), lset) =
	    processArgs (args, ins (lset, id), ins)

	(* Compute `Def' and `Kill' sets *)

	fun processArgs (OneArg id, set, x) = x (set, id)
	  | processArgs (TupArgs ids, set, x) =
	    List.app (fn id => x (set, id)) ids
	  | processArgs (ProdArgs labIdList, set, x) =
	    List.app (fn (_, id) => x (set, id)) labIdList

	fun insDef (set, IdDef (Id (_, stamp, _))) =
	    StampSet.insert (set, stamp)
	  | insDef (set, Wildcard) = ()

	fun insDefList (set, idDefs) =
	    List.app (fn idDef => insDef (set, idDef)) idDefs

	fun initStm (ValDec (_, idDef, exp), set) =
	    (insDef (set, idDef); initExp exp)
	  | initStm (RecDec (_, idDefExpList), set) =
	    List.app (fn (idDef, exp) => (insDef (set, idDef); initExp exp))
	    idDefExpList
	  | initStm (RefAppDec (_, idDef, _), set) = insDef (set, idDef)
	  | initStm (TupDec (_, idDefs, _), set) =
	    List.app (fn idDef => insDef (set, idDef)) idDefs
	  | initStm (ProdDec (_, labelIdDefList, _), set) =
	    List.app (fn (_, idDef) => insDef (set, idDef)) labelIdDefList
	  | initStm (RaiseStm (_, _), _) = ()
	  | initStm (ReraiseStm (_, _), _) = ()
	  | initStm (HandleStm (_, body1, idDef, body2, body3, _), set) =
	    let
		val set' = StampSet.clone set
	    in
		insDef (set', idDef);
		initBody (body1, StampSet.clone set);
		initBody (body2, set');
		initBody (body3, set)
	    end
	  | initStm (EndHandleStm (_, _), _) = ()
	  | initStm (TestStm (_, _, tests, body), set) =
	    let
		val set' = StampSet.clone set
	    in
		initTests (tests, set');
		initBody (body, set)
	    end
	  | initStm (SharedStm ({liveness = ref (Kill _), ...}, _, _), _) = ()
	  | initStm (SharedStm (_, body, _), set) = initBody (body, set)
	  | initStm (ReturnStm (_, exp), _) = initExp exp
	  | initStm (IndirectStm (_, ref bodyOpt), set) =
	    initBody (valOf bodyOpt, set)
	  | initStm (ExportStm (_, _), _) = ()
	and initTests (LitTests litBodyList, set) =
	    List.app (fn (_, body) => initBody (body, set)) litBodyList
	  | initTests (TagTests tagBodyList, set) =
	    List.app (fn (_, _, conArgs, body) =>
		      (case conArgs of
			   SOME args => processArgs (args, set, insDef)
			 | NONE => ();
		       initBody (body, set))) tagBodyList
	  | initTests (ConTests conBodyList, set) =
	    List.app (fn (_, conArgs, body) =>
		      (case conArgs of
			   SOME args => processArgs (args, set, insDef)
			 | NONE => ();
		       initBody (body, set))) conBodyList
	  | initTests (VecTests vecBodyList, set) =
	    List.app (fn (idDefs, body) =>
		      (insDefList (set, idDefs);
		       initBody (body, set))) vecBodyList
	and initExp (FunExp (_, _, _, args, body)) =
	    let
		val set = StampSet.new ()
	    in
		processArgs (args, set, insDef); initBody (body, set)
	    end
	  | initExp _ = ()
	and initBody (stm::stms, defSet) =
	    (case #liveness (infoStm stm) of
		 ref (Unknown | LoopStart | LoopEnd) =>
		     raise Crash.Crash "LivenessAnalysisPhase.initBody"
	       | r as ref (Use useSet) =>
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
	       | ref (Kill _) => ())
	  | initBody (nil, _) = ()

	fun translate () (_, component as (_, (body, _))) =
	    (scanBody (body, Copy (StampSet.new ()));
	     initBody (body, StampSet.new ());
	     component)
    end
