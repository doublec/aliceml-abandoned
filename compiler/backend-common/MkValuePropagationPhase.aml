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

signature SCOPED_IMP_MAP =
    sig
	type key
	type 'a map
	type 'a t = 'a map

	exception Collision of key
	exception Lookup of key

	val new: unit -> 'a map
	val cloneTop: 'a map -> 'a map
	val insertScope: 'a map -> unit
	val deleteScope: 'a map -> unit
	val insert: 'a map * key * 'a -> unit
	val insertDisjoint: 'a map * key * 'a -> unit   (* Collision *)
	val lookup: 'a map * key -> 'a option
	val lookupExistent: 'a map * key -> 'a   (* Lookup *)
	val appiScope: (key * 'a -> unit) -> 'a map -> unit
    end

functor MakeScopedImpMap(ImpMap: IMP_MAP) :>
    SCOPED_IMP_MAP where type key = ImpMap.key =
    struct
	type key = ImpMap.key
	type 'a map = 'a ImpMap.t list ref
	type 'a t = 'a map

	exception Collision = ImpMap.Collision
	exception Lookup = ImpMap.Lookup

	fun new () = ref [ImpMap.new ()]
	fun cloneTop (ref maps) =
	    ref (ImpMap.clone (List.hd maps)::List.tl maps)

	fun insertScope r = r := ImpMap.new ()::(!r)
	fun deleteScope r = r := List.tl (!r)

	fun insert (ref maps, key, entry) =
	    ImpMap.insert (List.hd maps, key, entry)
	fun insertDisjoint (ref maps, key, entry) =
	    ImpMap.insertDisjoint (List.hd maps, key, entry)

	fun lookup' (nil, _) = NONE
	  | lookup' ([map], key) = ImpMap.lookup (map, key)
	  | lookup' (map::rest, key) =
	    case ImpMap.lookup (map, key) of
		NONE => lookup' (rest, key)
	      | res as SOME _ => res

	fun lookup (ref maps, key) = lookup' (maps, key)

	fun lookupExistent (ref maps, key) =
	    case lookup' (maps, key) of
		SOME entry => entry
	      | NONE => raise Lookup key

	fun appiScope f (ref maps) = ImpMap.appi f (List.hd maps)
    end

functor MakeHashScopedImpMap(Key: HASH_KEY) =
	MakeScopedImpMap(MakeHashImpMap(Key))

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
	    fun node (edgeMap, stamp) =
		StampMap.insertDisjoint (edgeMap, stamp, nil)

	    fun edge (edgeMap, pred, succ) =
		let
		    val stamps = StampMap.lookupExistent (edgeMap, pred)
		in
		    StampMap.insert (edgeMap, pred, succ::stamps)
		end

	    fun sortStm (ValDec (_, _, _), _, _, _, _) = ()
	      | sortStm (RecDec (_, _), _, _, _, _) = ()
	      | sortStm (EvalStm (_, _), _, _, _, _) = ()
	      | sortStm (RaiseStm (_, _), _, _, _, _) = ()
	      | sortStm (ReraiseStm (_, _), _, _, _, _) = ()
	      | sortStm (HandleStm (_, body1, _, body2, body3, stamp),
			 pred, edgeMap, shared, path) =
		(node (edgeMap, stamp);
		 sortBody (body1, pred, edgeMap, shared, path);
		 sortBody (body2, pred, edgeMap, shared, path);
		 sortBody (body3, stamp, edgeMap, shared, path))
	      | sortStm (EndHandleStm (_, stamp), pred, edgeMap, _, _) =
		edge (edgeMap, pred, stamp)
	      | sortStm (TestStm (_, _, testBodyList, body),
			 pred, edgeMap, shared, path) =
		(List.app (fn (_, body) =>
			   sortBody (body, pred, edgeMap, shared, path))
		 testBodyList;
		 sortBody (body, pred, edgeMap, shared, path))
	      | sortStm (SharedStm (_, body, stamp),
			 pred, edgeMap, shared, path) =
		(StampMap.insert (shared, stamp,
				  if StampMap.member (shared, stamp)
				  then SHARED else UNIQUE);
		 if StampSet.member (path, stamp) then ()   (* ignore loops *)
		 else
		     (edge (edgeMap, pred, stamp);
		      sortSharedBody (body, stamp, edgeMap, shared, path)))
	      | sortStm (ReturnStm (_, _), _, _, _, _) = ()
	      | sortStm (IndirectStm (_, ref bodyOpt),
			 pred, edgeMap, shared, path) =
		sortBody (valOf bodyOpt, pred, edgeMap, shared, path)
	      | sortStm (ExportStm (_, _), _, _, _, _) = ()
	    and sortBody (stm::stms, pred, edgeMap, shared, path) =
		(sortStm (stm, pred, edgeMap, shared, path);
		 sortBody (stms, pred, edgeMap, shared, path))
	      | sortBody (nil, _, _, _, _) = ()
	    and sortSharedBody (body, stamp, edgeMap, shared, path) =
		if StampMap.member (edgeMap, stamp) then ()
		else
		    (node (edgeMap, stamp);
		     StampSet.insertDisjoint (path, stamp);
		     sortBody (body, stamp, edgeMap, shared, path);
		     StampSet.deleteExistent (path, stamp))

	    structure DepthFirstSearch =
		MakeDepthFirstSearch(structure Key = FromEqHashKey(Stamp)
				     structure Map = StampMap)
	in
	    fun sortShared (body, stamp) =
		let
		    val edgeMap = StampMap.new ()
		    val shared = StampMap.new ()
		    val path = StampSet.new ()
		in
		    sortSharedBody (body, stamp, edgeMap, shared, path);
		    (DepthFirstSearch.search edgeMap, shared)
		end
	end

	fun valueMin (value as EXP (_, stamp), EXP (_, stamp')) =
	    if stamp = stamp' then value else UNKNOWN
	  | valueMin (value as TEST (_, stamp), TEST (_, stamp')) =
	    if stamp = stamp' then value else UNKNOWN
	  | valueMin (_, _) = UNKNOWN

	fun unionEnv (env', env) =
	    IdMap.appiScope
	    (fn (id, entry as (value, isToplevel)) =>
	     case IdMap.lookup (env', id) of
		 SOME (value', isToplevel') =>
		     let
			 val entry' = (valueMin (value, value'), isToplevel)
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

	fun doSelTup (info, ids, n) = VarExp (info, List.nth (ids, n))

	fun doSelRec (info, labelIdList, n) =
	    let
		val (_, id) = List.nth (labelIdList, n)
	    in
		VarExp (info, id)
	    end

	fun doSel (info, label, n, id, env) =
	    case IdMap.lookupExistent (env, id) of
		((EXP (TupExp (_, ids), _) |
		  TEST (TupTest ids, _)), _) =>
		    doSelTup (info, ids, n)
	      | ((EXP (RecExp (_, labelIdList), _) |
		  TEST (RecTest labelIdList, _)), _) =>
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

	fun aliasArgs (OneArg id, OneArg id', env, isToplevel) =
	    alias (id, id', env, isToplevel)
	  | aliasArgs (TupArgs ids, TupArgs ids', env, isToplevel) =
	    ListPair.app (fn (id, id') => alias (id, id', env, isToplevel))
	    (ids, ids')
	  | aliasArgs (RecArgs labelIdList, RecArgs labelIdList',
		       env, isToplevel) =
	    ListPair.app (fn ((_, id), (_, id')) =>
			  alias (id, id', env, isToplevel))
	    (labelIdList, labelIdList')
	  | aliasArgs (_, _, _, _) =
	    raise Crash.Crash "ValuePropagationPhase.aliasArgs"

	datatype testRes =
	    ALWAYS_TRUE
	  | ALWAYS_FALSE
	  | DYNAMIC_TEST of test

	fun declareId (id, env, isToplevel) =
	    IdMap.insertDisjoint (env, id, (UNKNOWN, isToplevel))

	fun declareArgs (OneArg id, env, isToplevel) =
	    declareId (id, env, isToplevel)
	  | declareArgs (TupArgs ids, env, isToplevel) =
	    List.app (fn id => declareId (id, env, isToplevel)) ids
	  | declareArgs (RecArgs labelIdList, env, isToplevel) =
	    List.app (fn (_, id) => declareId (id, env, isToplevel))
	    labelIdList

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

	fun vpTest' (test, UNKNOWN, env, _) = dynamicTest (test, env)
	  | vpTest' (LitTest lit,
		     (EXP (LitExp (_, lit'), _) |
		      TEST (LitTest lit', _)), _, _) =
	    if lit = lit' then ALWAYS_TRUE else ALWAYS_FALSE
	  | vpTest' (TagTest (_, n),
		     (EXP (TagExp (_, _, n', _), _) |
		      TEST (TagTest (_, n'), _)), _, _) =
	    if n = n' then ALWAYS_TRUE else ALWAYS_FALSE
	  | vpTest' (TagAppTest (_, n, args),
		     (EXP (TagAppExp (_, _, n', args'), _) |
		      TEST (TagAppTest (_, n', args'), _)), env, isToplevel) =
	    if n = n' then
		(aliasArgs (args, args', env, isToplevel); ALWAYS_TRUE)
	    else ALWAYS_FALSE
	  | vpTest' (test as ConTest id,
		     (EXP (ConExp (_, id', _), _) |
		      TEST (ConTest id', _)), env, _) =
	    if idEq (id, id') then ALWAYS_TRUE else dynamicTest (test, env)
	  | vpTest' (test as ConAppTest (id, args),
		     (EXP (ConAppExp (_, id', args'), _) |
		      TEST (ConAppTest (id', args'), _)), env, isToplevel) =
	    if idEq (id, id') then
		(aliasArgs (args, args', env, isToplevel); ALWAYS_TRUE)
	    else dynamicTest (test, env)
	  | vpTest' (test as StaticConTest stamp,
		     (EXP (StaticConExp (_, stamp', _), _) |
		      TEST (StaticConTest stamp', _)), env, _) =
	    if stamp = stamp' then ALWAYS_TRUE else dynamicTest (test, env)
	  | vpTest' (test as StaticConAppTest (stamp, args),
		     (EXP (StaticConAppExp (_, stamp', args'), _) |
		      TEST (StaticConAppTest (stamp', args'), _)),
		     env, isToplevel) =
	    if stamp = stamp' then
		(aliasArgs (args, args', env, isToplevel); ALWAYS_TRUE)
	    else dynamicTest (test, env)
	  | vpTest' (TupTest ids,
		     (EXP (TupExp (_, ids'), _) |
		      TEST (TupTest ids', _)), env, isToplevel) =
	    (ListPair.app (fn (id, id') => alias (id, id', env, isToplevel))
	     (ids, ids'); ALWAYS_TRUE)
	  | vpTest' (RecTest labelIdList,
		     (EXP (RecExp (_, labelIdList'), _) |
		      TEST (RecTest labelIdList', _)), env, isToplevel) =
	    (ListPair.app (fn ((_, id), (_, id')) =>
			   alias (id, id', env, isToplevel))
	     (labelIdList, labelIdList'); ALWAYS_TRUE)
	  | vpTest' (test as LabTest (_, _, _), _, _, _) = DYNAMIC_TEST test
	  | vpTest' (VecTest ids,
		     (EXP (VecExp (_, ids'), _) |
		      TEST (VecTest ids', _)), env, isToplevel) =
	    if List.length ids = List.length ids' then
		(ListPair.app (fn (id, id') =>
			       alias (id, id', env, isToplevel))
		 (ids, ids'); ALWAYS_TRUE)
	    else ALWAYS_FALSE
	  | vpTest' (_, _, _, _) = ALWAYS_FALSE

	fun vpTest (test, id, env, isToplevel) =
	    let
		val value =
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
	    in
		vpTest' (test, value, env, isToplevel)
	    end

	fun indirect (_, [stm]) = stm
	  | indirect (info, body) = IndirectStm (info, ref (SOME body))

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
			 val entry = (EXP (exp, stamp), isToplevel)
		     in
			 IdMap.insertDisjoint (env, id, entry);
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
		val entry = SHARED_ANN (bodyOptRef, IdMap.cloneTop env)
		val _ = StampMap.insertDisjoint (shared, stamp, entry)
		val body1 = vpBodyScope (body1, env, isToplevel, shared)
		val _ = IdMap.insertDisjoint (env, id, (HANDLE, isToplevel))
		val body2 = vpBody (body2, env, isToplevel, shared)
		val info' = {region = #region info, liveness = ref Unknown}
		val body3 = [IndirectStm (info', bodyOptRef)]
	    in
		HandleStm (info, body1, id, body2, body3, stamp)
	    end
	  | vpStm (stm as EndHandleStm (_, stamp), env, _, shared) =
	    (case StampMap.lookupExistent (shared, stamp) of
		 SHARED_ANN (_, env') => (unionEnv (env', env); stm)
	       | _ => raise Crash.Crash "ValuePropagationPhase.vpStm")
	  | vpStm (stm as TestStm (info, id, _, _), env, isToplevel, shared) =
	    let
		val id = deref (id, env)
		val (testBodyList, elseBody) =
		    vpTestStm ([stm], id, env, isToplevel, shared)
	    in
		if List.null testBodyList then indirect (info, elseBody)
		else TestStm (info, id, testBodyList, elseBody)
	    end
	  | vpStm (SharedStm (info, body, stamp), env, isToplevel, shared) =
	    (case StampMap.lookupExistent (shared, stamp) of
		 UNIQUE =>
		     indirect (info, vpBody (body, env, isToplevel, shared))
	       | SHARED =>
		     let
			 val bodyOptRef = ref (SOME body)
			 val entry =
			     SHARED_ANN (bodyOptRef, IdMap.cloneTop env)
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
	  | vpStm (IndirectStm (info, ref bodyOpt), env, isToplevel, shared) =
	    indirect (info, vpBody (valOf bodyOpt, env, isToplevel, shared))
	  | vpStm (ExportStm (info, exp), env, isToplevel, shared) =
	    ExportStm (info, vpExp (exp, env, isToplevel, shared))
	and vpTestStm (topBody as [TestStm (_, id, [(test, body)], elseBody)],
		       id', env, isToplevel, shared) =
	    let
		val id = deref (id, env)
	    in
		if idEq (id, id') then
		    let
			val (testBodyList, elseBody) =
			    vpTestStm (elseBody, id', env, isToplevel, shared)
			val env = IdMap.cloneTop env
			val testRes = vpTest (test, id, env, isToplevel)
			val _ = declareTest (id, testRes, env, isToplevel)
			val body = vpBody (body, env, isToplevel, shared)
		    in
			case testRes of
			    ALWAYS_FALSE =>   (*--** warn? *)
				(testBodyList, elseBody)
			  | ALWAYS_TRUE => (nil, body)
			  | DYNAMIC_TEST test =>
				((test, body)::testBodyList, elseBody)
		    end
		else (nil, vpBodyScope (topBody, env, isToplevel, shared))
	    end
	  | vpTestStm (body as [SharedStm (_, body', stamp)],
		       id, env, isToplevel, shared) =
	    (case StampMap.lookupExistent (shared, stamp) of
		 UNIQUE => vpTestStm (body', id, env, isToplevel, shared)
	       | _ => (nil, vpBodyScope (body, env, isToplevel, shared)))
	  | vpTestStm ([IndirectStm (_, ref bodyOpt)],
		       id, env, isToplevel, shared) =
	    vpTestStm (valOf bodyOpt, id, env, isToplevel, shared)
	  | vpTestStm (body, _, env, isToplevel, shared) =
	    (nil, vpBodyScope (body, env, isToplevel, shared))
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
		val body = vpBodyShared (body, stamp, env, false)
		val _ = IdMap.deleteScope env
	    in
		FunExp (info, stamp, flags, args, body)
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
	and vpBodyScope (body, env, isToplevel, shared) =
	    vpBody (body, IdMap.cloneTop env, isToplevel, shared)
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
				   end;
			    StampMap.deleteExistent (shared, stamp)
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

	fun idToString (Id (_, stamp, Name.InId)) =
	    "$" ^ Stamp.toString stamp
	  | idToString (Id (_, stamp, Name.ExId s)) =
	    s ^ "$" ^ Stamp.toString stamp

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
		 case exn of
		     IdMap.Lookup id =>
			 TextIO.print ("Lookup " ^ idToString id ^ "\n")
		   | IdMap.Collision id =>
			 TextIO.print ("Collision " ^ idToString id ^ "\n")
		   | _ => ();
		 raise exn)
    end
