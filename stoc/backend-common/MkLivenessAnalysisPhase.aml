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
 * This file implements liveness analysis.  Its effect is to annotate
 * all statements in a program with liveness information, i.e., the
 * associated kill-sets (see below).  It does so in two phases:  First
 * the use-sets of all statements are computed (and annotated) during
 * a backwards traversal of the code.  Then the kill-sets are - in a
 * forward traversal of the code - computed from the annotated use-set
 * and the def-set of each statement.  The def-set is not annotated,
 * it is computed as we go along.
 *
 * Definitions:
 *
 * The /use-set/ of a statement S is the set of all identifiers that may
 * be referenced on any path starting from and including S and that have
 * already been initialized on (all) paths to S.
 *
 * The /def-set/ of a statement S is the set of all identifiers that will
 * already have been initialized upon reaching S.
 *
 * The /kill-set/ of a statement S is the set of identifiers that will not
 * be referenced in any path starting from S, and that are not in the
 * kill-set of any statement on a path leading to S.
 *)

structure LivenessAnalysisPhase1 :> LIVENESS_ANALYSIS_PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = FlatGrammar

	open I

	(* Annotate `use' set at each statement *)

	datatype 'a lazyCopy =
	    Orig of 'a
	  | Copy of 'a

	fun lazyValOf (Orig x) = x
	  | lazyValOf (Copy x) = x

	fun processArgs (OneArg id, lset, x) = x (lset, id)
	  | processArgs (TupArgs ids, lset, x) =
	    Vector.foldl (fn (id, lset) => x (lset, id)) lset ids
	  | processArgs (ProdArgs labIdVec, lset, x) =
	    Vector.foldl (fn ((_, id), lset) => x (lset, id)) lset labIdVec

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

	fun delDefVec (lset, idDefs) =
	    Vector.foldl (fn (idDef, lset) => delDef (lset, idDef)) lset idDefs

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

	fun insVec (lset, ids) =
	    Vector.foldl (fn (id, lset) => ins (lset, id)) lset ids

	fun union (Orig set, set') =
	    let
		val set'' = StampSet.clone set
	    in
		StampSet.union (set'', set');
		Copy set''
	    end
	  | union (lset as (Copy set), set') =
	    (StampSet.union (set, set'); lset)

	fun setInfo ({liveness = r as ref Unknown, ...}: stm_info, set) =
	    r := Use set
	  | setInfo ({liveness = ref (Use _), ...}, _) = ()
	  | setInfo ({liveness = ref (Kill _), ...}, _) =
	    raise Crash.Crash "LivenessAnalysisPhase.setInfo"

	fun scanBody (ValDec (i, idDef, exp)::stms) =
	    let
		val lset = scanBody stms
		val set = lazyValOf (scanExp (exp, delDef (lset, idDef)))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody (RecDec (i, idDefExpVec)::stms) =
	    let
		val lset = scanBody stms
		val lset' =
		    Vector.foldl (fn ((_, exp), lset) => scanExp (exp, lset))
		    lset idDefExpVec
		val set = lazyValOf lset'
	    in
		setInfo (i, set);
		Vector.foldl (fn ((idDef, _), lset) => delDef (lset, idDef))
		(Copy (StampSet.clone set)) idDefExpVec
	    end
	  | scanBody (RefAppDec (i, idDef, id)::stms) =
	    let
		val lset = scanBody stms
		val set = lazyValOf (ins (delDef (lset, idDef), id))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody (TupDec (i, idDefs, id)::stms) =
	    let
		val lset = scanBody stms
		val lset' =
		    Vector.foldr (fn (idDef, lset) => delDef (lset, idDef))
		    lset idDefs
		val set = lazyValOf (ins (lset', id))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody (ProdDec (i, labelIdDefVec, id)::stms) =
	    let
		val lset = scanBody stms
		val lset' =
		    Vector.foldr
		    (fn ((_, idDef), lset) => delDef (lset, idDef))
		    lset labelIdDefVec
		val set = lazyValOf (ins (lset', id))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [RaiseStm (i, Id (_, stamp, _))] =
	    let
		val set = StampSet.new ()
		val _ = StampSet.insert (set, stamp)
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [ReraiseStm (i, Id (_, stamp, _))] =
	    let
		val set = StampSet.new ()
		val _ = StampSet.insert (set, stamp)
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [TryStm (i, tryBody, idDef, handleBody)] =
	    let
		val lset1 = scanBody tryBody
		val lset2 = scanBody handleBody
		val set2 = lazyValOf (delDef (lset2, idDef))
		val set = lazyValOf (union (lset1, set2))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [EndTryStm (i, body)] =
	    let
		val set = lazyValOf (scanBody body)
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [EndHandleStm (i, body)] =
	    let
		val set = lazyValOf (scanBody body)
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [TestStm (i, id, tests, body)] =
	    let
		val lset1 = scanTests tests
		val lset2 = scanBody body
		val lset1' = ins (union (lset1, lazyValOf lset2), id)
		val set = lazyValOf lset1'
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [SharedStm (i as {liveness = r as ref Unknown, ...},
				 body, _)] =
	    let
		val set = lazyValOf (scanBody body)
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [SharedStm ({liveness = ref (Use set'), ...}, _, _)] =
	    Orig set'
	  | scanBody [SharedStm ({liveness = ref (Kill _), ...}, _, _)] =
	    raise Crash.Crash "LivenessAnalysisPhase.scanStm 1"
	  | scanBody [ReturnStm (i, exp)] =
	    let
		val set = lazyValOf (scanExp (exp, Copy (StampSet.new ())))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [IndirectStm (i, ref bodyOpt)] =
	    let
		val set = lazyValOf (scanBody (valOf bodyOpt))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody [ExportStm (i, exp)] =
	    let
		val set = lazyValOf (scanExp (exp, Copy (StampSet.new ())))
	    in
		setInfo (i, set);
		Orig set
	    end
	  | scanBody nil = Copy (StampSet.new ())
	  | scanBody _ = raise Crash.Crash "LivenessAnalysisPhase.scanStm 2"
	and scanTests (LitTests litBodyVec) =
	    (*--** this and the following folds can be improved *)
	    Vector.foldl (fn ((_, body), lset) =>
			  union (lset, lazyValOf (scanBody body)))
	    (Copy (StampSet.new ())) litBodyVec
	  | scanTests (TagTests tagBodyVec) =
	    Vector.foldl (fn ((_, _, conArgs, body), lset) =>
			  let
			      val lset' = scanBody body
			      val lset'' =
				  case conArgs of
				      SOME args =>
					  processArgs (args, lset', delDef)
				    | NONE => lset'
			  in
			      union (lset, lazyValOf lset'')
			  end) (Copy (StampSet.new ())) tagBodyVec
	  | scanTests (ConTests conBodyVec) =
	    Vector.foldl (fn ((_, conArgs, body), lset) =>
			  let
			      val lset' = scanBody body
			      val lset'' =
				  case conArgs of
				      SOME args =>
					  processArgs (args, lset', delDef)
				    | NONE => lset'
			  in
			      union (lset, lazyValOf lset'')
			  end) (Copy (StampSet.new ())) conBodyVec
	  | scanTests (VecTests vecBodyVec) =
	    Vector.foldl (fn ((idDefs, body), lset) =>
			  let
			      val lset' = scanBody body
			      val lset'' = delDefVec (lset, idDefs)
			  in
			      union (lset, lazyValOf lset'')
			  end) (Copy (StampSet.new ())) vecBodyVec
	and scanExp (LitExp (_, _), lset) = lset
	  | scanExp (PrimExp (_, _), lset) = lset
	  | scanExp (NewExp _, lset) = lset
	  | scanExp (VarExp (_, id), lset) = ins (lset, id)
	  | scanExp (TagExp (_, _, _), lset) = lset
	  | scanExp (ConExp (_, Con id), lset) = ins (lset, id)
	  | scanExp (ConExp (_, StaticCon _), lset) = lset
	  | scanExp (TupExp (_, ids), lset) = insVec (lset, ids)
	  | scanExp (ProdExp (_, labIdVec), lset) =
	    Vector.foldl (fn ((_, id), lset) => ins (lset, id)) lset labIdVec
	  | scanExp (VecExp (_, ids), lset) = insVec (lset, ids)
	  | scanExp (FunExp (_, _, _, args, body), lset) =
	    let
		val set = lazyValOf (scanBody body)
	    in
		processArgs (args, union (lset, set), delDef)
	    end
	  | scanExp (PrimAppExp (_, _, ids), lset) = insVec (lset, ids)
	  | scanExp (VarAppExp (_, id, args), lset) =
	    processArgs (args, ins (lset, id), ins)
	  | scanExp (TagAppExp (_, _, _, args), lset) =
	    processArgs (args, lset, ins)
	  | scanExp (ConAppExp (_, Con id, args), lset) =
	    processArgs (args, ins (lset, id), ins)
	  | scanExp (ConAppExp (_, StaticCon _, args), lset) =
	    processArgs (args, lset, ins)
	  | scanExp (RefAppExp (_, id), lset) = ins (lset, id)
	  | scanExp (SelAppExp (_, _, _, _, id), lset) = ins (lset, id)
	  | scanExp (FunAppExp (_, id, _, args), lset) =
	    processArgs (args, ins (lset, id), ins)

	fun translate () (_, component as (_, (body, _))) =
	    (scanBody body; component)
    end

structure LivenessAnalysisPhase2 :> LIVENESS_ANALYSIS_PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = FlatGrammar

	open I

	(* Annotate `kill' set at each statement *)

	fun processArgs (OneArg id, set, x) = x (set, id)
	  | processArgs (TupArgs ids, set, x) =
	    Vector.app (fn id => x (set, id)) ids
	  | processArgs (ProdArgs labIdVec, set, x) =
	    Vector.app (fn (_, id) => x (set, id)) labIdVec

	fun insDef (set, IdDef (Id (_, stamp, _))) =
	    StampSet.insert (set, stamp)
	  | insDef (set, Wildcard) = ()

	fun insDefVec (set, idDefs) =
	    Vector.app (fn idDef => insDef (set, idDef)) idDefs

	fun initStm (ValDec (_, idDef, exp), set) =
	    (insDef (set, idDef); initExp exp)
	  | initStm (RecDec (_, idDefExpVec), set) =
	    Vector.app (fn (idDef, exp) => (insDef (set, idDef); initExp exp))
	    idDefExpVec
	  | initStm (RefAppDec (_, idDef, _), set) = insDef (set, idDef)
	  | initStm (TupDec (_, idDefs, _), set) =
	    Vector.app (fn idDef => insDef (set, idDef)) idDefs
	  | initStm (ProdDec (_, labelIdDefVec, _), set) =
	    Vector.app (fn (_, idDef) => insDef (set, idDef)) labelIdDefVec
	  | initStm (RaiseStm (_, _), _) = ()
	  | initStm (ReraiseStm (_, _), _) = ()
	  | initStm (TryStm (_, tryBody, idDef, handleBody), set) =
	    let
		val set' = StampSet.clone set
	    in
		initBody (tryBody, StampSet.clone set);
		insDef (set', idDef);
		initBody (handleBody, set')
	    end
	  | initStm (EndTryStm (_, body), set) = initBody (body, set)
	  | initStm (EndHandleStm (_, body), set) = initBody (body, set)
	  | initStm (TestStm (_, _, tests, body), set) =
	    (initTests (tests, set); initBody (body, set))
	  | initStm (SharedStm ({liveness = ref (Kill _), ...}, _, _), _) = ()
	  | initStm (SharedStm (_, body, _), set) = initBody (body, set)
	  | initStm (ReturnStm (_, exp), _) = initExp exp
	  | initStm (IndirectStm (_, ref bodyOpt), set) =
	    initBody (valOf bodyOpt, set)
	  | initStm (ExportStm (_, _), _) = ()
	and initTests (LitTests litBodyVec, set) =
	    Vector.app (fn (_, body) => initBody (body, StampSet.clone set))
	    litBodyVec
	  | initTests (TagTests tagBodyVec, set) =
	    Vector.app (fn (_, _, conArgs, body) =>
			let
			    val set' = StampSet.clone set
			in
			    case conArgs of
				SOME args => processArgs (args, set', insDef)
			      | NONE => ();
			    initBody (body, set')
			end) tagBodyVec
	  | initTests (ConTests conBodyVec, set) =
	    Vector.app (fn (_, conArgs, body) =>
			let
			    val set' = StampSet.clone set
			in
			    case conArgs of
				SOME args => processArgs (args, set', insDef)
			      | NONE => ();
			    initBody (body, set')
			end) conBodyVec
	  | initTests (VecTests vecBodyVec, set) =
	    Vector.app (fn (idDefs, body) =>
			let
			    val set' = StampSet.clone set
			in
			    insDefVec (set', idDefs);
			    initBody (body, set')
			end) vecBodyVec
	and initExp (FunExp (_, _, _, args, body)) =
	    let
		val set = StampSet.new ()
	    in
		processArgs (args, set, insDef); initBody (body, set)
	    end
	  | initExp _ = ()
	and initBody (stm::stms, defSet) =
	    (case #liveness (infoStm stm) of
		 ref Unknown =>
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
	    (initBody (body, StampSet.new ()); component)
    end

functor MakeLivenessAnalysisPhase(Switches: SWITCHES) =
    let
	structure Phase1 =
	    MakeTracingPhase(structure Phase = LivenessAnalysisPhase1
			     structure Switches = Switches
			     val name = "Liveness Analysis - Pass 1")
	structure Phase1' =
	    MakeDumpingPhase(structure Phase = Phase1
			     structure Switches = Switches
			     val header = "Live Syntax with `use' sets"
			     val pp =
				 PrettyPrint.text
				 o OutputFlatGrammar.outputComponent
			     val switch =
				 Switches.Debug.dumpLivenessAnalysisIntermediate)

	structure Phase2 =
	    MakeTracingPhase(structure Phase = LivenessAnalysisPhase2
			     structure Switches = Switches
			     val name = "Liveness Analysis - Pass 2")
	structure Phase2' =
	    MakeDumpingPhase(structure Phase = Phase2
			     structure Switches = Switches
			     val header = "Live Syntax with `kill' sets"
			     val pp =
				 PrettyPrint.text
				 o OutputFlatGrammar.outputComponent
			     val switch =
				 Switches.Debug.dumpLivenessAnalysisResult)
    in
	ComposePhases(structure Phase1 = Phase1'
		      structure Phase2 = Phase2'
		      structure Context = EmptyContext
		      fun context1 () = ()
		      fun context2 () = ())
    end
