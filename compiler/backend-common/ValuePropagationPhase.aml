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

structure ValuePropagationPhase :> VALUE_PROPAGATION_PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = FlatGrammar

	open I

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

	datatype sharedEntry =
	    UNIQUE
	  | SHARED
	  | SHARED_ANN of body option ref * env

	local
	    fun node ((edgeMap, _), stamp) =
		StampMap.insertDisjoint (edgeMap, stamp, nil)

	    fun edge ((edgeMap, _), pred, succ) =
		let
		    val stamps = StampMap.lookupExistent (edgeMap, pred)
		in
		    StampMap.insert (edgeMap, pred, succ::stamps)
		end

	    fun sortStm (ValDec (_, _, _), _, _, _) = ()
	      | sortStm (RecDec (_, _), _, _, _) = ()
	      | sortStm (EvalStm (_, _), _, _, _) = ()
	      | sortStm (RaiseStm (_, _), _, _, _) = ()
	      | sortStm (ReraiseStm (_, _), _, _, _) = ()
	      | sortStm (HandleStm (_, body1, _, body2, body3, stamp),
			 pred, graph, path) =
		(node (graph, stamp);
		 sortBody (body1, pred, graph, path);
		 sortBody (body2, pred, graph, path);
		 sortBody (body3, stamp, graph, path))
	      | sortStm (EndHandleStm (_, stamp), pred, graph, _) =
		edge (graph, pred, stamp)
	      | sortStm (TestStm (_, _, testBodyList, body),
			 pred, graph, path) =
		(List.app (fn (_, body) =>
			   sortBody (body, pred, graph, path)) testBodyList;
		 sortBody (body, pred, graph, path))
	      | sortStm (SharedStm (_, body, stamp), pred,
			 graph as (edgeMap, shared), path) =
		(StampMap.insert (shared, stamp,
				  if StampMap.member (shared, stamp)
				  then SHARED else UNIQUE);
		 if StampSet.member (path, stamp) then ()   (* ignore loops *)
		 else
		     (edge (graph, pred, stamp);
		      if StampMap.member (edgeMap, stamp) then ()
		      else sortSharedBody (body, stamp, graph, path)))
	      | sortStm (ReturnStm (_, _), _, _, _) = ()
	      | sortStm (IndirectStm (_, ref bodyOpt), pred, graph, path) =
		sortBody (valOf bodyOpt, pred, graph, path)
	      | sortStm (ExportStm (_, _), _, _, _) = ()
	    and sortBody (stm::stms, pred, graph, path) =
		(sortStm (stm, pred, graph, path);
		 sortBody (stms, pred, graph, path))
	      | sortBody (nil, _, _, _) = ()
	    and sortSharedBody (body, stamp, graph, path) =
		(node (graph, stamp);
		 StampSet.insertDisjoint (path, stamp);
		 sortBody (body, stamp, graph, path);
		 StampSet.deleteExistent (path, stamp))

	    structure DepthFirstSearch =
		MakeDepthFirstSearch(structure Key = FromEqHashKey(Stamp)
				     structure Map = StampMap)
	in
	    fun sortShared (body, stamp) =
		let
		    val edgeMap = StampMap.new ()
		    val shared = StampMap.new ()
		in
		    sortSharedBody (body, stamp, (edgeMap, shared),
				    StampSet.new ());
		    (DepthFirstSearch.search edgeMap, shared)
		end
	end

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
	      | (EXP (exp as VarExp (info', id'), _), _) =>
		    getTerm (info', id', env)
	      | (EXP (exp as TagExp (_, _, _, _), _), _) => exp
	      | (EXP (exp as ConExp (_, _, _), _), _) => exp
	      | (EXP (exp as StaticConExp (_, _, _), _), _) => exp
	      | (EXP (exp as RefExp _, _), _) => exp
	      | (_, _) => VarExp (info, id)

	fun getConstruction (id, env) =
	    case IdMap.lookupExistent (env, id) of
		(value as EXP (LitExp (_, _), _), _) => value
	      | (value as EXP (NewExp (_, _), _), _) => value
	      | (value as EXP (TagExp (_, _, _, _), _), _) => value
	      | (value as EXP (ConExp (_, _, _), _), _) => value
	      | (value as EXP (StaticConExp (_, _, _), _), _) => value
	      | (value as EXP (TupExp (_, _), _), _) => value
	      | (value as EXP (RecExp (_, _), _), _) => value
	      | (value as EXP (VecExp (_, _), _), _) => value
	      | (value as TEST (_, _), _) => value
	      | (_, _) => UNKNOWN

	fun mapArgs f (OneArg id) = OneArg (f id)
	  | mapArgs f (TupArgs ids) = TupArgs (List.map f ids)
	  | mapArgs f (RecArgs labelIdList) =
	    RecArgs (List.map (fn (label, id) => (label, f id)) labelIdList)

	fun appArgs f (OneArg id) = f id
	  | appArgs f (TupArgs ids) = List.app f ids
	  | appArgs f (RecArgs labelIdList) =
	    List.app (fn (_, id) => f id) labelIdList

	fun deref (id, env) =
	    case IdMap.lookupExistent (env, id) of
		(EXP (VarExp (_, id'), _), _) => id'
	      | (_, _) => id

	fun derefArgs (args, env) = mapArgs (fn id => deref (id, env)) args

	fun gatherTests ([TestStm (_, id, testBodyList, elseBody)],
			 id', env, shared) =
	    let
		val id = deref (id, env)
	    in
		if idEq (id, id') then
		    let
			val (testBodyList', elseBody') =
			    gatherTests (elseBody, id', env, shared)
		    in
			(testBodyList @ testBodyList', elseBody')
		    end
		else (nil, elseBody)
	    end
(*--** still buggy
	  | gatherTests (body as [SharedStm (_, body', stamp)],
			 id, env, shared) =
	    (case StampMap.lookupExistent (shared, stamp) of
		 UNIQUE => gatherTests (body', id, env, shared)
	       | _ => (nil, body))
*)
	  | gatherTests ([IndirectStm (_, ref bodyOpt)], id, env, shared) =
	    gatherTests (valOf bodyOpt, id, env, shared)
	  | gatherTests (body, _, _, _) = (nil, body)

	fun doSelTup (info, ids, n) = VarExp (info, List.nth (ids, n))

	fun doSelRec (info, labelIdList, n) =
	    let
		val (_, id) = List.nth (labelIdList, n)
	    in
		VarExp (info, id)
	    end

	fun doSel (info, label, n, id, env) =
	    case IdMap.lookupExistent (env, id) of
		(EXP (TupExp (_, ids), _), _) =>
		    doSelTup (info, ids, n)
	      | (EXP (RecExp (_, labelIdList), _), _) =>
		    doSelRec (info, labelIdList, n)
	      | (TEST (TupTest ids, _), _) =>
		    doSelTup (info, ids, n)
	      | (TEST (RecTest labelIdList, _), _) =>
		    doSelRec (info, labelIdList, n)
	      | (_, _) => SelAppExp (info, label, n, id)

	fun arityMatches (OneArg _, SOME Unary) = true
	  | arityMatches (TupArgs _, SOME (TupArity _)) = true
	  | arityMatches (RecArgs _, SOME (RecArity _)) = true
	  | arityMatches (_, _) = false

	fun vpPrimApp (info, name, ids) =   (*--** evaluate partially *)
	    PrimAppExp (info, name, ids)

	fun primAppExp (info, _, name, Unary, args as OneArg id, _) =
	    vpPrimApp (info, name, [id])
	  | primAppExp (info, id, name, TupArity _, args as OneArg id', env) =
	    (case IdMap.lookupExistent (env, id') of
		 ((EXP (TupExp (_, ids), _) |
		   TEST (TupTest ids, _)), _) =>
		     vpPrimApp (info, name, ids)
	       | (_, _) => VarAppExp (info, id, args))   (*--** *)
	  | primAppExp (info, id, name, RecArity _, args as OneArg id', env) =
	    (case IdMap.lookupExistent (env, id') of
		 ((EXP (RecExp (_, labelIdList), _) |
		   TEST (RecTest labelIdList, _)), _) =>
		     vpPrimApp (info, name, List.map #2 labelIdList)
	       | (_, _) => VarAppExp (info, id, args))   (*--** *)
	  | primAppExp (info, id, name, TupArity _, TupArgs ids, _) =
	    vpPrimApp (info, name, ids)
	  | primAppExp (info, id, name, RecArity _, RecArgs labelIdList, _) =
	    vpPrimApp (info, name, List.map #2 labelIdList)
	  | primAppExp (info, id, _, _, args, _) =
	    VarAppExp (info, id, args)   (*--** *)

	fun alias (id, id' as Id (info', _, _), env, isToplevel) =
	    IdMap.insert
	    (env, id, (EXP (VarExp (info', id'), Stamp.new ()), isToplevel))

	fun matchArgs (OneArg id, OneArg id', env, isToplevel) =
	    alias (id, id', env, isToplevel)
	  | matchArgs (TupArgs ids, TupArgs ids', env, isToplevel) =
	    ListPair.app (fn (id, id') => alias (id, id', env, isToplevel))
	    (ids, ids')
	  | matchArgs (RecArgs labelIdList, RecArgs labelIdList',
		       env, isToplevel) =
	    ListPair.app (fn ((_, id), (_, id')) =>
			  alias (id, id', env, isToplevel))
	    (labelIdList, labelIdList')
	  | matchArgs (_, _, _, _) =
	    raise Crash.Crash "ValuePropagationPhase.matchArgs"

	datatype testRes =
	    ALWAYS_TRUE
	  | ALWAYS_FALSE
	  | DYNAMIC_TEST of test

	fun declareId (id, env, isToplevel) =
	    IdMap.insertDisjoint (env, id, (UNKNOWN, isToplevel))

	fun declareArgs (args, env, isToplevel) =
	    appArgs (fn id => declareId (id, env, isToplevel)) args

	fun declareTest' (LitTest _, _, _) = ()
	  | declareTest' (TagTest (_, _), _, _) = ()
	  | declareTest' (TagAppTest (_, _, args), env, isToplevel) =
	    declareArgs (args, env, isToplevel)
	  | declareTest' (ConTest _, _, _) = ()
	  | declareTest' (ConAppTest (_, args), env, isToplevel) =
	    declareArgs (args, env, isToplevel)
	  | declareTest' (StaticConTest _, _, _) = ()
	  | declareTest' (StaticConAppTest (_, args), env, isToplevel) =
	    declareArgs (args, env, isToplevel)
	  | declareTest' (RefAppTest id, env, isToplevel) =
	    declareId (id, env, isToplevel)
	  | declareTest' (TupTest ids, env, isToplevel) =
	    declareArgs (TupArgs ids, env, isToplevel)
	  | declareTest' (RecTest labelIdList, env, isToplevel) =
	    declareArgs (RecArgs labelIdList, env, isToplevel)
	  | declareTest' (LabTest (_, _, id), env, isToplevel) =
	    declareId (id, env, isToplevel)
	  | declareTest' (VecTest ids, env, isToplevel) =
	    declareArgs (TupArgs ids, env, isToplevel)

	fun declareTest (id, DYNAMIC_TEST test, env, isToplevel) =
	    (IdMap.insert (env, id, (TEST (test, Stamp.new ()), isToplevel));
	     declareTest' (test, env, isToplevel))
	  | declareTest (_, _, _, _) = ()

	fun dynamicTest (ConTest id, env) =
	    (case IdMap.lookupExistent (env, id) of
		 (EXP (StaticConExp (_, stamp, _), _), _) =>
		     DYNAMIC_TEST (StaticConTest stamp)
	       | (_, _) => DYNAMIC_TEST (ConTest (deref (id, env))))
	  | dynamicTest (ConAppTest (id, args), env) =
	    (case IdMap.lookupExistent (env, id) of
		 (EXP (StaticConExp (_, stamp, _), _), _) =>
		     DYNAMIC_TEST (StaticConAppTest (stamp, args))
	       | (_, _) => DYNAMIC_TEST (ConAppTest (deref (id, env), args)))
	  | dynamicTest (test, _) = DYNAMIC_TEST test

	fun vpTest (test, UNKNOWN, env, _) = dynamicTest (test, env)
	  | vpTest (LitTest lit,
		    (EXP (LitExp (_, lit'), _) |
		     TEST (LitTest lit', _)), _, _) =
	    if lit = lit' then ALWAYS_TRUE else ALWAYS_FALSE
	  | vpTest (TagTest (_, n),
		    (EXP (TagExp (_, _, n', _), _) |
		     TEST (TagTest (_, n'), _)), _, _) =
	    if n = n' then ALWAYS_TRUE else ALWAYS_FALSE
	  | vpTest (TagAppTest (_, n, args),
		    (EXP (TagAppExp (_, _, n', args'), _) |
		     TEST (TagAppTest (_, n', args'), _)), env, isToplevel) =
	    if n = n' then
		(matchArgs (args, args', env, isToplevel); ALWAYS_TRUE)
	    else ALWAYS_FALSE
	  | vpTest (test as ConTest id,
		    (EXP (ConExp (_, id', _), _) |
		     TEST (ConTest id', _)), env, _) =
	    if idEq (id, id') then ALWAYS_TRUE else dynamicTest (test, env)
	  | vpTest (test as ConAppTest (id, args),
		    (EXP (ConAppExp (_, id', args'), _) |
		     TEST (ConAppTest (id', args'), _)), env, isToplevel) =
	    if idEq (id, id') then
		(matchArgs (args, args', env, isToplevel); ALWAYS_TRUE)
	    else dynamicTest (test, env)
	  | vpTest (test as StaticConTest stamp,
		    (EXP (StaticConExp (_, stamp', _), _) |
		     TEST (StaticConTest stamp', _)), env, _) =
	    if stamp = stamp' then ALWAYS_TRUE else dynamicTest (test, env)
	  | vpTest (test as StaticConAppTest (stamp, args),
		    (EXP (StaticConAppExp (_, stamp', args'), _) |
		     TEST (StaticConAppTest (stamp', args'), _)),
		    env, isToplevel) =
	    if stamp = stamp' then
		(matchArgs (args, args', env, isToplevel); ALWAYS_TRUE)
	    else dynamicTest (test, env)
	  | vpTest (TupTest ids,
		    (EXP (TupExp (_, ids'), _) |
		     TEST (TupTest ids', _)), env, isToplevel) =
	    (ListPair.app (fn (id, id') => alias (id, id', env, isToplevel))
	     (ids, ids'); ALWAYS_TRUE)
	  | vpTest (RecTest labelIdList,
		    (EXP (RecExp (_, labelIdList'), _) |
		     TEST (RecTest labelIdList', _)), env, isToplevel) =
	    (ListPair.app (fn ((_, id), (_, id')) =>
			   alias (id, id', env, isToplevel))
	     (labelIdList, labelIdList'); ALWAYS_TRUE)
	  | vpTest (test as LabTest (_, _, _), _, _, _) = DYNAMIC_TEST test
	  | vpTest (VecTest ids,
		    (EXP (VecExp (_, ids'), _) |
		     TEST (VecTest ids', _)), env, isToplevel) =
	    if List.length ids = List.length ids' then
		(ListPair.app (fn (id, id') =>
			       alias (id, id', env, isToplevel))
		 (ids, ids'); ALWAYS_TRUE)
	    else ALWAYS_FALSE
	  | vpTest (_, _, _, _) = ALWAYS_FALSE

	fun vpStm (ValDec (info, id, exp), env, isToplevel, shared) =
	    let
		val exp = vpExp (exp, env, isToplevel, shared)
		val entry = (EXP (exp, Stamp.new ()), isToplevel)
	    in
		IdMap.insertDisjoint (env, id, entry);
		ValDec (info, id, exp)
	    end
	  | vpStm (RecDec (info, idExpList), env, isToplevel, shared) =
	    let
		val idExpStampList =
		    List.map
		    (fn (id, exp) =>
		     let
			 val stamp = Stamp.new ()
		     in
			 IdMap.insertDisjoint
			 (env, id, (EXP (exp, stamp), isToplevel));
			 (id, exp, Stamp.new ())
		     end) idExpList
		val idExpList =
		    List.map (fn (id, exp, stamp) =>
			      let
				  val exp =
				      vpExp (exp, env, isToplevel, shared)
				  val entry = (EXP (exp, stamp), isToplevel)
			      in
				  IdMap.insert (env, id, entry); (id, exp)
			      end) idExpStampList
	    in
		RecDec (info, idExpList)
	    end
	  | vpStm (EvalStm (info, exp), env, isToplevel, shared) =
	    EvalStm (info, vpExp (exp, env, isToplevel, shared))
	  | vpStm (stm as RaiseStm (info, id), env, _, _) =
	    let
		val id = deref (id, env)
	    in
		case IdMap.lookupExistent (env, id) of
		   (HANDLE, _) => ReraiseStm (info, id)
		 | _ => RaiseStm (info, id)
	    end
	  | vpStm (stm as ReraiseStm (info, id), env, _, _) =
	    ReraiseStm (info, deref (id, env))
	  | vpStm (HandleStm (info, body1, id, body2, body3, stamp),
		   env, isToplevel, shared) =
	    let
		val bodyOptRef = ref (SOME body3)
		val entry = SHARED_ANN (bodyOptRef, IdMap.clone env)
		val _ = StampMap.insertDisjoint (shared, stamp, entry)
		val body1 = vpBody (body1, env, isToplevel, shared)
		val _ = IdMap.insertScope env
		val _ = IdMap.insertDisjoint (env, id, (HANDLE, isToplevel))
		val body2 = vpBody (body2, env, isToplevel, shared)
		val _ = IdMap.deleteScope env
		val info' = {region = #region info, liveness = ref Unknown}
		val body3 = [IndirectStm (info', bodyOptRef)]
	    in
		HandleStm (info, body1, id, body2, body3, stamp)
	    end
	  | vpStm (stm as EndHandleStm (_, stamp), env, isToplevel, shared) =
	    (case StampMap.lookupExistent (shared, stamp) of
		 SHARED_ANN (_, env') =>
		     (unionEnv (env', env); stm)
	       | _ => raise Crash.Crash "ValuePropagationPhase.vpStm")
	  | vpStm (stm as TestStm (info, id, _, _), env, isToplevel, shared) =
	    let
		val id = deref (id, env)
		val (testBodyList, elseBody) =
		    gatherTests ([stm], id, env, shared)
		val _ = IdMap.insertScope env
		val elseBody = vpBody (elseBody, env, isToplevel, shared)
		val _ = IdMap.deleteScope env
		val (testBodyList, elseBody) =
		    List.foldr
		    (fn ((test, body), (rest, elseBody)) =>
		     let
			 val _ = IdMap.insertScope env
			 val value = getConstruction (id, env)
			 val testRes = vpTest (test, value, env, isToplevel)
			 val _ = declareTest (id, testRes, env, isToplevel)
			 val body = vpBody (body, env, isToplevel, shared)
			 val _ = IdMap.deleteScope env
		     in
			 case testRes of
			     ALWAYS_FALSE => (rest, elseBody)   (*--** warn? *)
			   | ALWAYS_TRUE => (nil, body)
			   | DYNAMIC_TEST test =>
				 ((test, body)::rest, elseBody)
		     end) (nil, elseBody) testBodyList
	    in
		case testBodyList of
		    _::_ => TestStm (info, id, testBodyList, elseBody)
		  | nil => IndirectStm (info, ref (SOME elseBody))
	    end
	  | vpStm (SharedStm (info, body, stamp), env, isToplevel, shared) =
	    (case StampMap.lookupExistent (shared, stamp) of
		 UNIQUE =>
		     let
			 val body' = vpBody (body, env, isToplevel, shared)
		     in
			 IndirectStm (info, ref (SOME body'))
		     end
	       | SHARED =>
		     let
			 val bodyOptRef = ref (SOME body)
			 val entry = SHARED_ANN (bodyOptRef, IdMap.clone env)
			 val info' = {region = #region info,
				      liveness = ref Unknown}
		     in
			 StampMap.insert (shared, stamp, entry);
			 SharedStm (info, [IndirectStm (info', bodyOptRef)],
				    stamp)
		     end
	       | SHARED_ANN (bodyOptRef, env') =>
		     let
			 val _ = unionEnv (env', env)
			 val info' = {region = #region info,
				      liveness = ref Unknown}
		     in
			 SharedStm (info, [IndirectStm (info', bodyOptRef)],
				    stamp)
		     end)
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
	  | vpExp (exp as TagExp (_, _, _, _), _, _, _) = exp
	  | vpExp (ConExp (info, id, conArity), env, _, _) =
	    let
		val id = deref (id, env)
	    in
		case IdMap.lookupExistent (env, id) of
		    (EXP (exp as StaticConExp (_, _, _), _), _) => exp
		  | (_, _) => ConExp (info, id, conArity)
	    end
	  | vpExp (exp as StaticConExp (_, _, _), _, _, _) = exp
	  | vpExp (exp as RefExp _, _, _, _) = exp
	  | vpExp (TupExp (info, ids), env, _, _) =
	    (*--** if TupExp took terms instead of ids -> getTerm *)
	    TupExp (info, List.map (fn id => deref (id, env)) ids)
	  | vpExp (RecExp (info, labelIdList), env, _, _) =
	    (*--** if RecExp took terms instead of ids -> getTerm *)
	    RecExp (info, List.map (fn (label, id) => (label, deref (id, env)))
		    labelIdList)
	  | vpExp (exp as SelExp (_, _, _), _, _, _) = exp
	  | vpExp (VecExp (info, ids), env, _, _) =
	    (*--** if VecExp took terms instead of ids -> getTerm *)
	    VecExp (info, List.map (fn id => deref (id, env)) ids)
	  | vpExp (FunExp (info, stamp, flags, args, body), env, _, _) =
	    let
		val _ = IdMap.insertScope env
		val _ = declareArgs (args, env, false)
		val body' = vpBodyShared (body, stamp, env, false)
		val _ = IdMap.deleteScope env
	    in
		FunExp (info, stamp, flags, args, body')
	    end
	  | vpExp (PrimAppExp (info, name, ids), env, _, _) =
	    vpPrimApp (info, name, List.map (fn id => deref (id, env)) ids)
	  | vpExp (VarAppExp (info, id, args), env, _, _) =
	    let
		val id = deref (id, env)
		val args = derefArgs (args, env)
	    in
		case IdMap.lookupExistent (env, id) of
		    (EXP (PrimExp (_, name), _), _) =>
			primAppExp (info, deref (id, env), name,
				    valOf (PrimOps.getArity name), args, env)
		  | (EXP (TagExp (_, label, n, conArity), _), _) =>
			if arityMatches (args, conArity) then
			    TagAppExp (info, label, n, args)
			else VarAppExp (info, id, args)
		  | (EXP (ConExp (_, id, conArity), _), _) =>
			if arityMatches (args, conArity) then
			    ConAppExp (info, id, args)
			else VarAppExp (info, id, args)
		  | (EXP (StaticConExp (_, stamp, conArity), _), _) =>
			if arityMatches (args, conArity) then
			    StaticConAppExp (info, stamp, args)
			else VarAppExp (info, id, args)
		  | (EXP (RefExp _, _), _) =>
			(case args of
			     OneArg id => RefAppExp (info, id)
			   | _ => VarAppExp (info, id, args))   (*--** *)
		  | (EXP (SelExp (_, label, n), _), _) =>
			(case derefArgs (args, env) of
			     OneArg id =>
				 doSel (info, label, n, id, env)
			   | TupArgs ids =>
				 doSelTup (info, ids, n)
			   | RecArgs labelIdList =>
				 doSelRec (info, labelIdList, n))
		  | (EXP (FunExp (_, stamp, _, _, _), _), true) =>
			(*--** optimize args conversion *)
			FunAppExp (info, id, stamp, args)
		  | (_, _) => VarAppExp (info, id, args)
	    end
	  | vpExp (TagAppExp (info, label, n, args), env, _, _) =
	    TagAppExp (info, label, n, derefArgs (args, env))
	  | vpExp (ConAppExp (info, id, args), env, _, _) =
	    let
		val id = deref (id, env)
	    in
		case IdMap.lookupExistent (env, id) of
		    (EXP (StaticConExp (_, stamp, _), _), _) =>
			StaticConAppExp (info, stamp, derefArgs (args, env))
		  | (_, _) => ConAppExp (info, id, derefArgs (args, env))
	    end
	  | vpExp (StaticConAppExp (info, stamp, args), env, _, _) =
	    StaticConAppExp (info, stamp, derefArgs (args, env))
	  | vpExp (RefAppExp (info, id), env, _, _) =
	    RefAppExp (info, deref (id, env))
	  | vpExp (SelAppExp (info, label, n, id), env, _, _) =
	    doSel (info, label, n, deref (id, env), env)
	  | vpExp (FunAppExp (info, id, stamp, args), env, _, _) =
	    FunAppExp (info, id, stamp, derefArgs (args, env))
	and vpBody (stm::stms, env, isToplevel, shared) =
	    vpStm (stm, env, isToplevel, shared)::
	    vpBody (stms, env, isToplevel, shared)
	  | vpBody (nil, _, _, _) = nil
	and vpBodyShared (body, stamp, env, isToplevel) =
	    (case sortShared (body, stamp) of
		 ([stamp']::sorted, shared) =>
		     (Assert.assert (stamp = stamp');
		      vpBody (body, env, isToplevel, shared) before
		      List.app
		      (fn stamps =>
		       let
			   val _ = Assert.assert (List.null (List.tl stamps))
			   val stamp = List.hd stamps
		       in
			   case StampMap.lookupExistent (shared, stamp) of
			       UNIQUE => ()
			     | SHARED =>
				   raise Crash.Crash
				       "ValuePropagationPhase.vpBodyShared 1"
			     | SHARED_ANN (bodyOptRef, env) =>
				   let
				       val body = valOf (!bodyOptRef)
				   in
				       bodyOptRef :=
				       SOME (vpBody (body, env, isToplevel,
						     shared))
				   end
		       end) sorted)
	       | (_, _) =>
		     raise Crash.Crash "ValuePropagationPhase.vpBodyShared 2")

	fun newEnv () =
	    let
		val env = IdMap.new ()
		val info = {region = Source.nowhere}
		fun ins (stamp, name, value) =
		    IdMap.insertDisjoint
		    (env, Id ({region = Source.nowhere}, stamp, name), value)
		fun exp exp = (EXP (exp, Stamp.new ()), true)
		open Prebound
	    in
		ins (valstamp_false, valname_false,
		     (exp (TagExp (info, Label.fromString "false", 0, NONE))));
		ins (valstamp_true, valname_true,
		     (exp (TagExp (info, Label.fromString "true", 1, NONE))));
		ins (valstamp_nil, valname_nil,
		     (exp (TagExp (info, Label.fromString "nil", 1, NONE))));
		ins (valstamp_cons, valname_cons,
		     (exp (TagExp (info, Label.fromString "::", 0, NONE))));
		ins (valstamp_ref, valname_ref,
		     (exp (RefExp info)));
		(*--** use StaticConExps: *)
		ins (valstamp_match, valname_match, (UNKNOWN, true));
		ins (valstamp_bind, valname_bind, (UNKNOWN, true));
		env
	    end

	fun debug component =
	    TextIO.print
	    ("\n" ^ OutputFlatGrammar.outputComponent component ^ "\n")

	fun translate () (_, component as (imports, (body, sign))) =
	    let
		val env = newEnv ()
		val _ =
		    List.app (fn (id, _, _) =>
			      declareId (id, env, true)) imports
		val topStamp = Stamp.new ()
		val body' = vpBodyShared (body, topStamp, env, true)
		val component' = (imports, (body', sign))
	    in
		component'
	    end
	    handle exn =>
		(TextIO.print
		 "\nValuePropagationPhase crashed: \
		 \debug information follows\n";
		 debug component;
		 raise exn)
    end
