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

(*
 * Optimizations:
 * -- do constant hoisting and selection lifting (TBD)
 * -- distinguish application types
 * -- do constant folding on PrimAppExps (TBD)
 * -- parallelize tests
 * -- propagate toplevel constructors of open datatypes
 * -- delay and reorder initializations (TBD: DeadCodeEliminationPhase)
 * -- inline and specialize functions (TBD)
 * -- remove unused imports (TBD: DeadCodeEliminationPhase)
 * -- partially evaluate selections
 * -- remove side-effect-free parts of EvalStms (TBD: DeadCodeEliminationPhase)
 *
 * Cleanups and debug support:
 * -- remove args/conArity mismatches (TBD)
 * -- compute print names for functions and generic constructors (TBD)
 * -- eliminate bogus SharedStms and IndirectStms (TBD)
 * -- raise-to-reraise transformation (TBD)
 *)

structure ValuePropagationPhase :> VALUE_PROPAGATION_PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = FlatGrammar

	open I

	local
	    fun sortStm (ValDec (_, _, _), _, _, _) = ()
	      | sortStm (RecDec (_, _), _, _, _) = ()
	      | sortStm (EvalStm (_, _), _, _, _) = ()
	      | sortStm (RaiseStm (_, _), _, _, _) = ()
	      | sortStm (ReraiseStm (_, _), _, _, _) = ()
	      | sortStm (HandleStm (_, body1, _, body2, body3, _),
			 pred, map, path) =
		(sortBody (body1, pred, map, path);
		 sortBody (body2, pred, map, path);
		 sortBody (body3, pred, map, path))
	      | sortStm (EndHandleStm (_, _), _, _, _) = ()
	      | sortStm (TestStm (_, _, testBodyList, body), pred, map, path) =
		(List.app (fn (_, body) =>
			   sortBody (body, pred, map, path)) testBodyList;
		 sortBody (body, pred, map, path))
	      | sortStm (SharedStm (_, body, stamp), pred, map, path) =
		if StampSet.member (path, stamp) then ()   (* ignore loops *)
		else
		    let
			val stamps = StampMap.lookupExistent (map, pred)
		    in
			StampMap.insert (map, pred, stamp::stamps);
			if StampMap.member (map, stamp) then ()
			else sortSharedBody (body, stamp, map, path)
		    end
	      | sortStm (ReturnStm (_, _), _, _, _) = ()
	      | sortStm (IndirectStm (_, ref bodyOpt), pred, map, path) =
		sortBody (valOf bodyOpt, pred, map, path)
	      | sortStm (ExportStm (_, _), _, _, _) = ()
	    and sortBody (stm::stms, pred, map, path) =
		(sortStm (stm, pred, map, path);
		 sortBody (stms, pred, map, path))
	      | sortBody (nil, _, _, _) = ()
	    and sortSharedBody (body, stamp, map, path) =
		(StampMap.insertDisjoint (map, stamp, nil);
		 StampSet.insertDisjoint (path, stamp);
		 sortBody (body, stamp, map, path);
		 StampSet.deleteExistent (path, stamp))

	    structure DepthFirstSearch =
		MakeDepthFirstSearch(structure Key = FromEqHashKey(Stamp)
				     structure Map = StampMap)
	in
	    fun sortShared (body, stamp) =
		let
		    val map: stamp list StampMap.t = StampMap.new ()
		in
		    sortSharedBody (body, stamp, map, StampSet.new ());
		    DepthFirstSearch.search map
		end
	end

	fun idEq (Id (_, stamp1, _), Id (_, stamp2, _)) = stamp1 = stamp2

	structure IdMap =
	    MakeHashScopedImpMap(type t = id
				 val equals = idEq
				 fun hash (Id (_, stamp, _)) =
				     Stamp.hash stamp)

	datatype value =
	    EXP of exp * stamp
	  | TEST of test * stamp
	  | HANDLE
	  | UNKNOWN
	type isToplevel = bool
	type env = (value * isToplevel) IdMap.t

	datatype testRes =
	    ALWAYS_TRUE of stm list
	  | ALWAYS_FALSE
	  | DYNAMIC_TEST of test

	structure StampMap = MakeHashScopedImpMap(FromEqHashKey(Stamp))

	type shared = (body option ref * env * isToplevel) StampMap.t

	fun valueMin (value as EXP (_, stamp), EXP (_, stamp')) =
	    if stamp = stamp' then value else UNKNOWN
	  | valueMin (value as TEST (_, stamp), TEST (_, stamp')) =
	    if stamp = stamp' then value else UNKNOWN
	  | valueMin (_, _) = UNKNOWN

	fun unionEnv (env', env) =
	    IdMap.appi (fn (id, entry as (value, isToplevel)) =>
			case IdMap.lookup (env', id) of
			    SOME (value', isToplevel') =>
				let
				    val entry' =
					(valueMin (value, value'), isToplevel)
				in
				    Assert.assert (isToplevel = isToplevel');
				    IdMap.insert (env', id, entry')
				end
			  | NONE => IdMap.insertDisjoint (env', id, entry)) env

	fun getTerm (info, id, env) =
	    case IdMap.lookupExistent (env, id) of
		(EXP (exp as LitExp (_, _), _), _) => exp
	      | (EXP (exp as VarExp (_, _), _), _) => exp
	      | (EXP (exp as TagExp (_, _, _), _), _) => exp
	      | (EXP (exp as ConExp (_, _, _), _), _) => exp
	      | (EXP (exp as StaticConExp (_, _, _), _), _) => exp
	      | (EXP (exp as RefExp _, _), _) => exp
	      | (_, _) => VarExp (info, id)

	fun getConstruction (id, env) =
	    case IdMap.lookupExistent (env, id) of
		(value as EXP (LitExp (_, _), _), _) => value
	      | (value as EXP (NewExp (_, _), _), _) => value
	      | (value as EXP (TagExp (_, _, _), _), _) => value
	      | (value as EXP (ConExp (_, _, _), _), _) => value
	      | (value as EXP (StaticConExp (_, _, _), _), _) => value
	      | (value as EXP (TupExp (_, _), _), _) => value
	      | (value as EXP (RecExp (_, _), _), _) => value
	      | (value as EXP (VecExp (_, _), _), _) => value
	      | (value as TEST (_, _), _) => value
	      | (_, _) => UNKNOWN

	fun deref (id, env) =
	    case IdMap.lookupExistent (env, id) of
		(EXP (VarExp (_, id'), _), _) => id'
	      | (_, _) => id

	fun derefArgs (OneArg id, env) = OneArg (deref (id, env))
	  | derefArgs (TupArgs ids, env) =
	     TupArgs (List.map (fn id => deref (id, env)) ids)
	  | derefArgs (RecArgs labelIdList, env) =
	    RecArgs (List.map (fn (label, id) => (label, deref (id, env)))
		     labelIdList)

	fun appArgs f (OneArg id) = f id
	  | appArgs f (TupArgs ids) = List.app f ids
	  | appArgs f (RecArgs labelIdList) =
	    List.app (fn (_, id) => f id) labelIdList

	fun gatherTests ([TestStm (_, id, testBodyList, elseBody)], id', env) =
	    let
		val id'' = deref (id, env)
	    in
		if idEq (id'', id') then
		    let
			val (testBodyList', elseBody') =
			    gatherTests (elseBody, id', env)
		    in
			(testBodyList @ testBodyList', elseBody')
		    end
		else (nil, elseBody)
	    end
	  | gatherTests (body, _, _) = (nil, body)

	fun labelToInt label = LargeInt.toInt (valOf (Label.toLargeInt label))

	fun doSel (info, label, id, env) =
	    case IdMap.lookupExistent (env, id) of
		(EXP (TupExp (_, ids), _), _) =>
		    VarExp (info, List.nth (ids, labelToInt label - 1))
	      | (EXP (RecExp (_, labelIdList), _), _) =>
		    VarExp (info, #2 (valOf (List.find (fn (label', _) =>
							label = label')
					     labelIdList)))
	      | (TEST (TupTest ids, _), _) =>
		    VarExp (info, List.nth (ids, labelToInt label - 1))
	      | (TEST (RecTest labelIdList, _), _) =>
		    VarExp (info, #2 (valOf (List.find (fn (label', _) =>
							label = label')
					     labelIdList)))
	      | (_, _) => SelAppExp (info, label, id)

	fun vpTest (test, value, env) = DYNAMIC_TEST test   (*--** implement *)

(*--** The implementation of vpTest will be along the lines of:

	fun matchArgs (OneArg id1, OneArg id2, env)
	    IdMap.insertDisjoint
	    (id1, (EXP (VarExp (info, id2), Stamp.new ()), isToplevel))
	  | ...

	fun vpTest (test, UNKNOWN, _) = DYNAMIC_TEST test
	  | vpTest (LitTest lit,
		    (EXP (LitExp lit', _) |
		     TEST (LitTest lit', _)), _) =
	    if lit = lit' then ALWAYS_TRUE nil else ALWAYS_FALSE
	  | vpTest (TagTest label,
		    (EXP (TagExp (_, label', _), _) |
		     TEST (TagTest label', _)), _) =
	    if label = label' then ALWAYS_TRUE nil else ALWAYS_FALSE
	  | vpTest (TagAppTest (label, args, _),
		    (EXP (TagAppExp (_, label', args', _)) |
		     TEST (TagAppTest (label', args', _))), env) =
	    if label = label' then matchArgs (args, args', env)
	    else ALWAYS_FALSE
	  | ...
	  | vpTest (_, _, _) = ALWAYS_FALSE
*)

	fun vpStm (ValDec (info, id, exp), env, isToplevel, shared) =
	    let
		val exp' = vpExp (exp, env, isToplevel, shared)
		val entry = (EXP (exp', Stamp.new ()), isToplevel)
	    in
		IdMap.insertDisjoint (env, id, entry);
		ValDec (info, id, exp')
	    end
	  | vpStm (RecDec (info, idExpList), env, isToplevel, shared) =
	    let
		val idExpStampList =
		    List.map (fn (id, exp) => (id, exp, Stamp.new ()))
		    idExpList
		val _ =
		    List.app (fn (id, exp, stamp) =>
			      IdMap.insertDisjoint
			      (env, id, (EXP (exp, stamp), isToplevel)))
		    idExpStampList
		val idExpList' =
		    List.map (fn (id, exp, stamp) =>
			      let
				  val exp' =
				      vpExp (exp, env, isToplevel, shared)
				  val entry = (EXP (exp', stamp), isToplevel)
			      in
				  IdMap.insert (env, id, entry); (id, exp')
			      end) idExpStampList
	    in
		RecDec (info, idExpList')
	    end
	  | vpStm (EvalStm (info, exp), env, isToplevel, shared) =
	    (*--** remove if free of side-effects: DeadCodeEliminationPhase *)
	    EvalStm (info, vpExp (exp, env, isToplevel, shared))
	  | vpStm (stm as RaiseStm (info, id), env, _, _) =
	    (*--** transform to reraise *)
	    RaiseStm (info, deref (id, env))
	  | vpStm (stm as ReraiseStm (info, id), env, _, _) =
	    ReraiseStm (info, deref (id, env))
	  | vpStm (HandleStm (info, body1, id, body2, body3, stamp),
		   env, isToplevel, shared) =
	    let
		val body1' = vpBody (body1, env, isToplevel, shared)
		val _ = IdMap.insertScope env
		val _ = IdMap.insertDisjoint (env, id, (HANDLE, isToplevel))
		val body2' = vpBody (body2, env, isToplevel, shared)
		val _ = IdMap.deleteScope env
		val body3' = vpBody (body3, env, isToplevel, shared)
	    in
		HandleStm (info, body1', id, body2', body3', stamp)
	    end
	  | vpStm (stm as EndHandleStm (_, _), _, _, _) = stm
	  | vpStm (stm as TestStm (info, id, _, _), env, isToplevel, shared) =
	    let
		val id = deref (id, env)
		val (testBodyList, body) = gatherTests ([stm], id, env)
		val testBodyList' =
		    List.mapPartial
		    (fn (test, body) =>
		     let
			 val env' = IdMap.clone env
		     in
			 case vpTest (test, getConstruction (id, env), env') of
			     ALWAYS_FALSE => NONE   (*--** warning? *)
			   | ALWAYS_TRUE body =>   (*--** implement *)
				 SOME (test,
				       vpBody (body, env', isToplevel, shared))
			   | DYNAMIC_TEST test' =>
				 (*--** insert TEST entry into env *)
				 SOME (test,
				       vpBody (body, env', isToplevel, shared))
		     end) testBodyList
		val body' = vpBody (body, env, isToplevel, shared)
	    in
		case testBodyList' of
		    _::_ => TestStm (info, id, testBodyList', body')
		  | nil => IndirectStm (info, ref (SOME body'))
	    end
	  | vpStm (SharedStm (info, body, stamp), env, isToplevel, shared) =
	    (case StampMap.lookup (shared, stamp) of
		 NONE =>
		     let
			 val bodyOptRef = ref (SOME body)
			 val entry = (bodyOptRef, IdMap.clone env, isToplevel)
		     in
			 StampMap.insertDisjoint (shared, stamp, entry);
			 IndirectStm (info, bodyOptRef)
		     end
	       | SOME (bodyOptRef, env', _) =>
		     (unionEnv (env', env);
		      IndirectStm (info, bodyOptRef)))
	  | vpStm (ReturnStm (info, exp), env, isToplevel, shared) =
	    ReturnStm (info, vpExp (exp, env, isToplevel, shared))
	  | vpStm (stm as IndirectStm (info, bodyOptRef as ref bodyOpt),
		   env, isToplevel, shared) =
	    let
		val body' = vpBody (valOf bodyOpt, env, isToplevel, shared)
	    in
		bodyOptRef := SOME body'; stm
	    end
	  | vpStm (ExportStm (info, exp), env, isToplevel, shared) =
	    ExportStm (info, vpExp (exp, env, isToplevel, shared))
	and vpExp (exp as LitExp (_, _), _, _, _) = exp
	  | vpExp (exp as PrimExp (_, _), _, _, _) = exp
	  | vpExp (exp as NewExp (_, _), _, false, _) = exp
	  | vpExp (NewExp (info, conArity), _, true, _) =
	    StaticConExp (info, Stamp.new (), conArity)
	  | vpExp (VarExp (info, id), env, _, _) = getTerm (info, id, env)
	  | vpExp (exp as TagExp (_, _, _), _, _, _) = exp
	  | vpExp (ConExp (info, id, conArity), env, _, _) =
	    (case IdMap.lookupExistent (env, id) of
		 (EXP (exp' as StaticConExp (_, _, _), _), _) => exp'
	       | (_, _) => ConExp (info, deref (id, env), conArity))
	  | vpExp (exp as StaticConExp (_, _, _), _, _, _) = exp
	  | vpExp (exp as RefExp _, _, _, _) = exp
	  | vpExp (TupExp (info, ids), env, _, _) =
	    (*--** if TupExp took terms instead of ids -> getTerm *)
	    TupExp (info, List.map (fn id => deref (id, env)) ids)
	  | vpExp (RecExp (info, labelIdList), env, _, _) =
	    (*--** if RecExp took terms instead of ids -> getTerm *)
	    RecExp (info, List.map (fn (label, id) => (label, deref (id, env)))
		    labelIdList)
	  | vpExp (exp as SelExp (_, _), _, _, _) = exp
	  | vpExp (VecExp (info, ids), env, _, _) =
	    (*--** if VecExp took terms instead of ids -> getTerm *)
	    VecExp (info, List.map (fn id => deref (id, env)) ids)
	  | vpExp (FunExp (info, stamp, flags, args, body), env, _, shared) =
	    (*--** honor instantiate flag *)
	    let
		val _ = IdMap.insertScope env
		val _ = StampMap.insertScope shared
		val _ =
		    appArgs
		    (fn id => IdMap.insertDisjoint (env, id, (UNKNOWN, false)))
		    args
		val body' = vpBodyShared (body, stamp, env, false, shared)
		val _ = IdMap.deleteScope env
		val _ = StampMap.deleteScope shared
	    in
		FunExp (info, stamp, flags, args, body')
	    end
	  | vpExp (PrimAppExp (info, string, ids), env, _, _) =
	    (*--** partially evaluate this application *)
	    PrimAppExp (info, string, List.map (fn id => deref (id, env)) ids)
	  | vpExp (exp as VarAppExp (info, id, args), env, _, _) =
	    (case IdMap.lookupExistent (env, id) of
		 (EXP (PrimExp (_, string), _), _) =>
		     (case derefArgs (args, env) of
			  TupArgs ids => PrimAppExp (info, string, ids)
			| _ => raise Crash.Crash "ValuePropagationPhase.vpExp")
	       | (EXP (VarExp (_, id'), _), _) =>
		     VarAppExp (info, id', derefArgs (args, env))
	       | (EXP (TagExp (_, label, conArity), _), _) =>
		     TagAppExp (info, label, derefArgs (args, env), conArity)
	       | (EXP (ConExp (_, id, conArity), _), _) =>
		     ConAppExp (info, id, derefArgs (args, env), conArity)
	       | (EXP (StaticConExp (_, stamp, conArity), _), _) =>
		     StaticConAppExp (info, stamp, derefArgs (args, env),
				      conArity)
	       | (EXP (RefExp _, _), _) =>
		     (case derefArgs (args, env) of
			  OneArg id => RefAppExp (info, id)
			| _ => exp)   (*--** conArity mismatch *)
	       | (EXP (SelExp (_, label), _), _) =>
		     (case derefArgs (args, env) of
			  OneArg id => doSel (info, label, id, env)
			| TupArgs ids =>
			      VarExp (info,
				      List.nth (ids, labelToInt label - 1))
			| RecArgs labelIdList =>
			      VarExp (info,
				      #2 (valOf (List.find (fn (label', _) =>
							    label = label')
						 labelIdList))))
	       | (EXP (FunExp (_, stamp, _, _, _), _), true) =>
		     (*--** optimize args conversion *)
		     FunAppExp (info, id, stamp, derefArgs (args, env))
	       | (_, _) => exp)
	  | vpExp (TagAppExp (info, label, args, conArity), env, _, _) =
	    (*--** resolve conArity mismatches *)
	    TagAppExp (info, label, derefArgs (args, env), conArity)
	  | vpExp (ConAppExp (info, id, args, conArity), env, _, _) =
	    (*--** resolve conArity mismatches *)
	    (case IdMap.lookupExistent (env, id) of
		 (EXP (StaticConExp (_, stamp, _), _), _) =>
		      StaticConAppExp (info, stamp, derefArgs (args, env),
				       conArity)
	       | (_, _) =>
		      ConAppExp (info, id, derefArgs (args, env), conArity))
	  | vpExp (StaticConAppExp (info, stamp, args, conArity), env, _, _) =
	    (*--** resolve conArity mismatches *)
	    StaticConAppExp (info, stamp, derefArgs (args, env), conArity)
	  | vpExp (RefAppExp (info, id), env, _, _) =
	    RefAppExp (info, deref (id, env))
	  | vpExp (SelAppExp (info, label, id), env, _, _) =
	    doSel (info, label, id, env)
	  | vpExp (FunAppExp (info, id, stamp, args), env, _, _) =
	    FunAppExp (info, id, stamp, derefArgs (args, env))
	  | vpExp (AdjExp (info, id1, id2), env, _, _) =
	    (*--** evaluate partially *)
	    AdjExp (info, deref (id1, env), deref (id2, env))
	and vpBody (stm::stms, env, isToplevel, shared) =
	    vpStm (stm, env, isToplevel, shared)::
	    vpBody (stms, env, isToplevel, shared)
	  | vpBody (nil, _, _, _) = nil
	and vpBodyShared (body, stamp, env, isToplevel, shared) =
	    (case sortShared (body, stamp) of
		 [stamp']::sorted =>
		     (Assert.assert (stamp = stamp');
		      vpBody (body, env, isToplevel, shared) before
		      List.app
		      (fn stamps =>
		       let
			   val _ = Assert.assert (List.null (List.tl stamps))
			   val stamp = List.hd stamps
			   val (bodyOptRef, env, isToplevel) =
			       StampMap.lookupExistent (shared, stamp)
			   val body = valOf (!bodyOptRef)
		       in
			   bodyOptRef :=
			   SOME (vpBody (body, env, isToplevel, shared))
		       end) sorted)
	       | _ => raise Crash.Crash "ValuePropagationPhase.vpBodyShared")

	fun translate () (imports, (body, sign)) =
	    let
		val body' =
		    vpBodyShared (body, Stamp.new (), IdMap.new (),
				  true, StampMap.new ())
	    in
		(imports, (body', sign))
	    end
    end
