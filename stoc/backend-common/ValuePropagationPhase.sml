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

	exception Lookup of key

	val new: unit -> 'a map
	val cloneTop: 'a map -> 'a map
	val insertScope: 'a map -> unit
	val deleteScope: 'a map -> unit
	val insert: 'a map * key * 'a -> unit
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

	exception Lookup = ImpMap.Lookup

	fun new () = ref [ImpMap.new ()]
	fun cloneTop (ref maps) =
	    ref (ImpMap.clone (List.hd maps)::List.tl maps)

	fun insertScope r = r := ImpMap.new ()::(!r)
	fun deleteScope r = r := List.tl (!r)

	fun insert (ref maps, key, entry) =
	    ImpMap.insert (List.hd maps, key, entry)

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

	fun isId (IdDef _) = true
	  | isId (Wildcard) = false

	fun idOf (IdDef id) = id
	  | idOf (Wildcard) = raise Crash.Crash "ValuePropagationPhase.idOf"

	structure IdMap =
	    MakeHashScopedImpMap(type t = id
				 val equals = idEq
				 fun hash (Id (_, stamp, _)) =
				     Stamp.hash stamp)

	datatype value =
	    LitVal of lit
	  | PrimVal of string
	  | VarVal of id
	  | TagVal of label * int * Arity.t option
	  | ConVal of con * Arity.t option
	  | RefVal
	  | TupVal of idDef vector
	  | ProdVal of (label * idDef) vector
	  | SelVal of label * int
	  | VecVal of idDef vector
	  | FunVal of stamp * funFlag list
	  | TagAppVal of label * int * idDef args
	  | ConAppVal of con * idDef args
	  | RefAppVal of idDef
	  | CaughtExnVal
	  | UnknownVal

	fun mapArgs f (OneArg x) = OneArg (f x)
	  | mapArgs f (TupArgs xs) = TupArgs (Vector.map f xs)
	  | mapArgs f (ProdArgs labelXVec) =
	    ProdArgs (Vector.map (fn (label, x) => (label, f x)) labelXVec)

	fun expToValue (LitExp (_, lit)) = LitVal lit
	  | expToValue (PrimExp (_, name)) = PrimVal name
	  | expToValue (NewExp _) = UnknownVal   (*--** ConVal? *)
	  | expToValue (VarExp (_, id)) = VarVal id
	  | expToValue (TagExp (_, label, n)) = TagVal (label, n, NONE)
	  | expToValue (ConExp (_, con)) = ConVal (con, NONE)
	  | expToValue (TupExp (_, ids)) = TupVal (Vector.map IdDef ids)
	  | expToValue (ProdExp (_, labelIdVec)) =
	    ProdVal (Vector.map (fn (label, id) =>
				 (label, IdDef id)) labelIdVec)
	  | expToValue (VecExp (_, ids)) = VecVal (Vector.map IdDef ids)
	  | expToValue (FunExp (_, stamp, funFlags, _, _)) =
	    FunVal (stamp, funFlags)
	  | expToValue (PrimAppExp (_, _, _)) = UnknownVal
	  | expToValue (VarAppExp (_, _, _)) = UnknownVal
	  | expToValue (TagAppExp (_, label, n, args)) =
	    TagAppVal (label, n, mapArgs IdDef args)
	  | expToValue (ConAppExp (_, con, args)) =
	    ConAppVal (con, mapArgs IdDef args)
	  | expToValue (RefAppExp (_, id)) = RefAppVal (IdDef id)
	  | expToValue (SelAppExp (_, _, _, _)) = UnknownVal
	  | expToValue (FunAppExp (_, _, _, _)) = UnknownVal

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
	      | sortStm (RefAppDec (_, _, _), _, _, _, _) = ()
	      | sortStm (TupDec (_, _, _), _, _, _, _) = ()
	      | sortStm (ProdDec (_, _, _), _, _, _, _) = ()
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
	      | sortStm (TestStm (_, _, tests, body),
			 pred, edgeMap, shared, path) =
		(sortTests (tests, pred, edgeMap, shared, path);
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
	    and sortTests (LitTests litBodyVec, pred, edgeMap, shared, path) =
		Vector.app (fn (_, body) =>
			    sortBody (body, pred, edgeMap, shared, path))
		litBodyVec
	      | sortTests (TagTests tagBodyVec, pred, edgeMap, shared, path) =
		Vector.app (fn (_, _, _, body) =>
			    sortBody (body, pred, edgeMap, shared, path))
		tagBodyVec
	      | sortTests (ConTests conBodyVec, pred, edgeMap, shared, path) =
		Vector.app (fn (_, _, body) =>
			    sortBody (body, pred, edgeMap, shared, path))
		conBodyVec
	      | sortTests (VecTests vecBodyVec, pred, edgeMap, shared, path) =
		Vector.app (fn (_, body) =>
			    sortBody (body, pred, edgeMap, shared, path))
		vecBodyVec
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

	fun idDefEq (IdDef (Id (_, stamp, _)), IdDef (Id (_, stamp', _))) =
	    stamp = stamp'
	  | idDefEq (Wildcard, Wildcard) = true
	  | idDefEq (_, _) = false

	fun conEq (Con (Id (_, stamp, _)), Con (Id (_, stamp', _))) =
	    stamp = stamp'
	  | conEq (StaticCon stamp, StaticCon stamp') = stamp = stamp'
	  | conEq (_, _) = false

	fun argsMin (args as OneArg idDef, OneArg idDef') =
	    if idDefEq (idDef, idDef') then args else OneArg Wildcard
	  | argsMin (TupArgs idDefs, TupArgs idDefs') =
	    TupArgs (VectorPair.map (fn (idDef, idDef') =>
				     if idDefEq (idDef, idDef') then idDef
				     else Wildcard) (idDefs, idDefs'))
	  | argsMin (ProdArgs labelIdDefVec, ProdArgs labelIdDefVec') =
	    ProdArgs (VectorPair.map (fn ((label, idDef), (_, idDef')) =>
				      (label,
				       if idDefEq (idDef, idDef') then idDef
				       else Wildcard))
		      (labelIdDefVec, labelIdDefVec'))
	  | argsMin (_, _) = raise Crash.Crash "ValuePropagationPhase.argsMin"

	fun valueMin (value as LitVal lit, LitVal lit') =
	    if lit = lit' then value else UnknownVal
	  | valueMin (value as PrimVal name, PrimVal name') =
	    if name = name' then value else UnknownVal
	  | valueMin (value as VarVal (Id (_, stamp, _)),
		      VarVal (Id (_, stamp', _))) =
	    if stamp = stamp' then value else UnknownVal
	  | valueMin (value as TagVal (label, _, _), TagVal (label', _, _)) =
	    if label = label' then value else UnknownVal
	  | valueMin (value as ConVal (con, _), ConVal (con', _)) =
	    if conEq (con, con') then value else UnknownVal
	  | valueMin (RefVal, RefVal) = RefVal
	  | valueMin (TupVal idDefs, TupVal idDefs') =
	    TupVal (VectorPair.map (fn (idDef, idDef') =>
				    if idDefEq (idDef, idDef') then idDef
				    else Wildcard) (idDefs, idDefs'))
	  | valueMin (ProdVal labelIdDefVec, ProdVal labelIdDefVec') =
	    ProdVal (VectorPair.map (fn ((label, idDef), (_, idDef')) =>
				     (label, if idDefEq (idDef, idDef')
					     then idDef else Wildcard))
		     (labelIdDefVec, labelIdDefVec'))
	  | valueMin (value as SelVal (label, _), SelVal (label', _)) =
	    if label = label' then value else UnknownVal
	  | valueMin (VecVal idDefs, VecVal idDefs') =
	    if Vector.length idDefs = Vector.length idDefs' then
		VecVal (VectorPair.map (fn (idDef, idDef') =>
					if idDefEq (idDef, idDef') then idDef
					else Wildcard) (idDefs, idDefs'))
	    else UnknownVal
	  | valueMin (value as FunVal (stamp, _), FunVal (stamp', _)) =
	    if stamp = stamp' then value else UnknownVal
	  | valueMin (TagAppVal (label, n, args),
		      TagAppVal (label', _, args')) =
	    if label = label' then TagAppVal (label, n, argsMin (args, args'))
	    else UnknownVal
	  | valueMin (value as RefAppVal idDef, RefAppVal idDef') =
	    if idDefEq (idDef, idDef') then value else UnknownVal
	  | valueMin (CaughtExnVal, CaughtExnVal) = CaughtExnVal
	  | valueMin (_, _) = UnknownVal

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
	       | NONE => IdMap.insert (env', id, entry)) env

	fun getTerm (info, id, env) =
	    case IdMap.lookupExistent (env, id) of
		(LitVal lit, _) => LitExp (info, lit)
	      | (PrimVal name, _) => PrimExp (info, name)
	      | (VarVal id', _) => getTerm (info, id', env)
	      | (TagVal (label, n, NONE), _) => TagExp (info, label, n)
	      | (ConVal (con, NONE), _) => ConExp (info, con)
	      | (_, _) => VarExp (info, id)

	fun deref (id, env) =
	    case IdMap.lookupExistent (env, id) of
		(VarVal id', _) => id'
	      | (_, _) => id

	fun derefArgs (OneArg id, env) = OneArg (deref (id, env))
	  | derefArgs (TupArgs ids, env) =
	    TupArgs (Vector.map (fn id => deref (id, env)) ids)
	  | derefArgs (ProdArgs labelIdVec, env) =
	    ProdArgs (Vector.map (fn (label, id) => (label, deref (id, env)))
		      labelIdVec)

	fun doSel (info, label, n, id, env) =
	    case IdMap.lookupExistent (env, id) of
		(TupVal idDefs, _) =>
		    (case Vector.sub (idDefs, n) of
			 IdDef id => VarExp (info, id)
		       | Wildcard => SelAppExp (info, label, n, id))
	      | (ProdVal labelIdDefVec, _) =>
		    (case Vector.sub (labelIdDefVec, n) of
			 (_, IdDef id) => VarExp (info, id)
		       | (_, Wildcard) => SelAppExp (info, label, n, id))
	      | (_, _) => SelAppExp (info, label, n, id)

	fun arityMatches (OneArg _, SOME Arity.Unary) = true
	  | arityMatches (TupArgs _, SOME (Arity.Tuple _)) = true
	  | arityMatches (ProdArgs _, SOME (Arity.Product _)) = true
	  | arityMatches (_, _) = false

	fun vpPrimApp (info, name, ids) =   (*--** evaluate partially *)
	    (*--** assertion about arity *)
	    PrimAppExp (info, name, ids)

	fun primAppExp (info, _, name, Arity.Unary, args as OneArg id, _) =
	    vpPrimApp (info, name, #[id])
	  | primAppExp (info, id, name,
			Arity.Tuple _, args as OneArg id', env) =
	    (case IdMap.lookupExistent (env, id') of
		 (TupVal idDefs, _) =>
		     if Vector.all isId idDefs then
			 vpPrimApp (info, name, Vector.map idOf idDefs)
		     else VarAppExp (info, id, args)
	       | (_, _) => VarAppExp (info, id, args))   (*--** *)
	  | primAppExp (info, id, name,
			Arity.Product _, args as OneArg id', env) =
	    (case IdMap.lookupExistent (env, id') of
		 (ProdVal labelIdDefVec, _) =>
		     if Vector.all (fn (_, idDef) => isId idDef) labelIdDefVec
		     then
			 vpPrimApp (info, name,
				    Vector.map (fn (_, idDef) => idOf idDef)
				    labelIdDefVec)
		     else VarAppExp (info, id, args)
	       | (_, _) => VarAppExp (info, id, args))   (*--** *)
	  | primAppExp (info, id, name, Arity.Tuple _, TupArgs ids, _) =
	    vpPrimApp (info, name, ids)
	  | primAppExp (info, id, name,
			Arity.Product _, ProdArgs labelIdVec, _) =
	    vpPrimApp (info, name, Vector.map #2 labelIdVec)
	  | primAppExp (info, id, _, _, args, _) =
	    VarAppExp (info, id, args)   (*--** crash? *)

	fun alias (IdDef id, IdDef id', env, isToplevel) =
	    IdMap.insert (env, id, (VarVal id', isToplevel))
	  | alias (_, _, _, _) = ()

	fun aliasArgs (OneArg idDef, OneArg idDef', env, isToplevel) =
	    alias (idDef, idDef', env, isToplevel)
	  | aliasArgs (TupArgs idDefs, TupArgs idDefs', env, isToplevel) =
	    VectorPair.app (fn (idDef, idDef') =>
			    alias (idDef, idDef', env, isToplevel))
	    (idDefs, idDefs')
	  | aliasArgs (ProdArgs labelIdDefVec, ProdArgs labelIdDefVec',
		       env, isToplevel) =
	    VectorPair.app (fn ((_, idDef), (_, idDef')) =>
			    alias (idDef, idDef', env, isToplevel))
	    (labelIdDefVec, labelIdDefVec')
	  | aliasArgs (_, _, _, _) =
	    raise Crash.Crash "ValuePropagationPhase.aliasArgs"

	fun declare (env, IdDef id, entry) = IdMap.insert (env, id, entry)
	  | declare (_, Wildcard, _) = ()

	fun declareUnknown (env, idDef, isToplevel) =
	    declare (env, idDef, (UnknownVal, isToplevel))

	fun declareArgs (env, OneArg idDef, isToplevel) =
	    declareUnknown (env, idDef, isToplevel)
	  | declareArgs (env, TupArgs idDefs, isToplevel) =
	    Vector.app (fn idDef =>
			declareUnknown (env, idDef, isToplevel)) idDefs
	  | declareArgs (env, ProdArgs labelIdDefVec, isToplevel) =
	    Vector.app (fn (_, idDef) =>
			declareUnknown (env, idDef, isToplevel)) labelIdDefVec

	fun declareConArgs (_, NONE, _) = ()
	  | declareConArgs (env, SOME args, isToplevel) =
	    declareArgs (env, args, isToplevel)

	fun testsAppend (NONE, testsOpt) = testsOpt
	  | testsAppend (testsOpt, NONE) = testsOpt
	  | testsAppend (SOME (LitTests xs), SOME (LitTests ys)) =
	    SOME (LitTests (Vector.append (xs, ys)))
	  | testsAppend (SOME (TagTests xs), SOME (TagTests ys)) =
	    SOME (TagTests (Vector.append (xs, ys)))
	  | testsAppend (SOME (ConTests xs), SOME (ConTests ys)) =
	    SOME (ConTests (Vector.append (xs, ys)))
	  | testsAppend (SOME (VecTests xs), SOME (VecTests ys)) =
	    SOME (VecTests (Vector.append (xs, ys)))
	  | testsAppend (SOME _, SOME _) =
	    raise Crash.Crash "ValuePropagationPhase.testsAppend"

	fun testsNull (SOME (LitTests xs)) = Vector.length xs = 0
	  | testsNull (SOME (TagTests xs)) = Vector.length xs = 0
	  | testsNull (SOME (ConTests xs)) = Vector.length xs = 0
	  | testsNull (SOME (VecTests xs)) = Vector.length xs = 0
	  | testsNull NONE = true

	fun indirect (_, [stm]) = stm
	  | indirect (info, body) = IndirectStm (info, ref (SOME body))

	fun vpStm (ValDec (info, idDef, exp), env, isToplevel, shared) =
	    let
		val exp = vpExp (exp, env, isToplevel, shared)
	    in
		declare (env, idDef, (expToValue exp, isToplevel));
		ValDec (info, idDef, exp)
	    end
	  | vpStm (RecDec (info, idDefExpVec), env, isToplevel, shared) =
	    let
		val _ =
		    Vector.app (fn (idDef, exp) =>
				declare (env, idDef, (expToValue exp,
						      isToplevel))) idDefExpVec
		val idDefExpVec =
		    Vector.map (fn (idDef, exp) =>
				let
				    val exp =
					vpExp (exp, env, isToplevel, shared)
				in
				    declare (env, idDef, (expToValue exp,
							  isToplevel));
				    (idDef, exp)
				end) idDefExpVec
	    in
		RecDec (info, idDefExpVec)
	    end
	  | vpStm (RefAppDec (info, idDef, id), env, isToplevel, _) =
	    let
		val id = deref (id, env)
	    in
		IdMap.insert (env, id, (RefAppVal idDef, isToplevel));
		declareUnknown (env, idDef, isToplevel);
		RefAppDec (info, idDef, id)
	    end
	  | vpStm (TupDec (info, idDefs, id), env, isToplevel, _) =
	    let
		val id = deref (id, env)
		val idDefs =
		    case IdMap.lookupExistent (env, id) of
			(TupVal idDefs', isToplevel') =>
			    VectorPair.map
			    (fn (idDef, idDef') =>
			     case (idDef, idDef') of
				 (IdDef id, IdDef id') =>
				     (IdMap.insert (env, id,
						    (VarVal id', isToplevel'));
				      Wildcard)
			       | (_, _) => idDef) (idDefs, idDefs')
		      | (_, _) => idDefs
	    in
		Vector.app (fn idDef =>
			    declareUnknown (env, idDef, isToplevel)) idDefs;
		IdMap.insert (env, id, (TupVal idDefs, isToplevel));
		TupDec (info, idDefs, id)
	    end
	  | vpStm (ProdDec (info, labelIdDefVec, id), env, isToplevel, _) =
	    let
		val id = deref (id, env)
		val labelIdDefVec =
		    case IdMap.lookupExistent (env, id) of
			(ProdVal labelIdDefVec', isToplevel') =>
			    VectorPair.map
			    (fn ((_, idDef), labelIdDef' as (label, idDef')) =>
			     case (idDef, idDef') of
				 (IdDef id, IdDef id') =>
				     (IdMap.insert (env, id,
						    (VarVal id', isToplevel'));
				      (label, Wildcard))
			       | (_, _) => labelIdDef')
			    (labelIdDefVec, labelIdDefVec')
		      | (_, _) => labelIdDefVec
	    in
		Vector.app (fn (_, idDef) =>
			    declareUnknown (env, idDef, isToplevel))
		labelIdDefVec;
		IdMap.insert (env, id, (ProdVal labelIdDefVec, isToplevel));
		ProdDec (info, labelIdDefVec, id)
	    end
	  | vpStm (stm as RaiseStm (info, id), env, _, _) =
	    let
		val id = deref (id, env)
	    in
		case IdMap.lookupExistent (env, id) of
		   (CaughtExnVal, _) => ReraiseStm (info, id)
		 | _ => RaiseStm (info, id)
	    end
	  | vpStm (stm as ReraiseStm (info, id), env, _, _) =
	    ReraiseStm (info, deref (id, env))
	  | vpStm (HandleStm (info, body1, idDef, body2, body3, stamp),
		   env, isToplevel, shared) =
	    let
		val bodyOptRef = ref (SOME body3)
		val entry = SHARED_ANN (bodyOptRef, IdMap.cloneTop env)
		val _ = StampMap.insertDisjoint (shared, stamp, entry)
		val body1 = vpBodyScope (body1, env, isToplevel, shared)
		val _ = declare (env, idDef, (CaughtExnVal, isToplevel))
		val body2 = vpBody (body2, env, isToplevel, shared)
		val info' = {region = #region info, liveness = ref Unknown}
		val body3 = [IndirectStm (info', bodyOptRef)]
	    in
		HandleStm (info, body1, idDef, body2, body3, stamp)
	    end
	  | vpStm (stm as EndHandleStm (_, stamp), env, _, shared) =
	    (case StampMap.lookupExistent (shared, stamp) of
		 SHARED_ANN (_, env') => (unionEnv (env', env); stm)
	       | _ => raise Crash.Crash "ValuePropagationPhase.vpStm")
	  | vpStm (stm as TestStm (info, id, _, _), env, isToplevel, shared) =
	    let
		val id = deref (id, env)
		val (testsOpt, elseBody) =
		    vpTestStm ([stm], id, env, isToplevel, shared)
	    in
		if testsNull testsOpt then indirect (info, elseBody)
		else TestStm (info, id, valOf testsOpt, elseBody)
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
	and vpTestStm (topBody as [TestStm (_, id, tests, elseBody)],
		       id', env, isToplevel, shared) =
	    let
		val id = deref (id, env)
	    in
		if idEq (id, id') then
		    let
			val (testsOpt, elseBody) =
			    vpTestStm (elseBody, id', env, isToplevel, shared)
			val (testsOpt', elseBody) =
			    vpTests (id, tests, elseBody,
				     env, isToplevel, shared)
		    in
			(testsAppend (testsOpt', testsOpt), elseBody)
		    end
		else (NONE, vpBodyScope (topBody, env, isToplevel, shared))
	    end
	  | vpTestStm (body as [SharedStm (_, body', stamp)],
		       id, env, isToplevel, shared) =
	    (case StampMap.lookupExistent (shared, stamp) of
		 UNIQUE => vpTestStm (body', id, env, isToplevel, shared)
	       | _ => (NONE, vpBodyScope (body, env, isToplevel, shared)))
	  | vpTestStm ([IndirectStm (_, ref bodyOpt)],
		       id, env, isToplevel, shared) =
	    vpTestStm (valOf bodyOpt, id, env, isToplevel, shared)
	  | vpTestStm (body, _, env, isToplevel, shared) =
	    (NONE, vpBodyScope (body, env, isToplevel, shared))
	and vpTests (id, LitTests litBodyVec,
		     elseBody, env, isToplevel, shared) =
	    let
		val (litBodyList, elseBody) =
		    Vector.foldr
		    (fn ((lit, body), (litBodyList, elseBody)) =>
		     let
			 val env = IdMap.cloneTop env
			 val entry = IdMap.lookupExistent (env, id)
			 val entry' = (LitVal lit, isToplevel)
			 val _ = IdMap.insert (env, id, entry')
			 val body = vpBody (body, env, isToplevel, shared)
		     in
			 case entry of
			     (LitVal lit', _) =>
				 if lit = lit' then (nil, body)
				 else (nil, elseBody)
			   | _ => ((lit, body)::litBodyList, elseBody)
		     end) (nil, elseBody) litBodyVec
	    in
		(case litBodyList of
		     nil => NONE
		   | _::_ => SOME (LitTests (Vector.fromList litBodyList)),
		 elseBody)
	    end
	  | vpTests (id, TagTests tagBodyVec,
		     elseBody, env, isToplevel, shared) =
	    let
		val (tagBodyList, elseBody) =
		    Vector.foldr
		    (fn ((label, n, conArgs, body), (tagBodyList, elseBody)) =>
		     let
			 val env = IdMap.cloneTop env
			 val entry = IdMap.lookupExistent (env, id)
			 val entry' =
			     (case conArgs of
				  NONE => TagVal (label, n, NONE)
				| SOME args => TagAppVal (label, n, args),
			      isToplevel)
			 val _ = IdMap.insert (env, id, entry')
			 val _ = declareConArgs (env, conArgs, isToplevel)
		     in
			 case (conArgs, entry) of
			     (NONE, (TagVal (_, n', _), _)) =>
				 if n = n' then
				     (nil, vpBody (body, env,
						   isToplevel, shared))
				 else (nil, elseBody)
			   | (SOME args, (TagAppVal (_, n', args'), _)) =>
				 if n = n' then
				     (aliasArgs (args, args', env, isToplevel);
				      (nil, vpBody (body, env,
						    isToplevel, shared)))
				 else (nil, elseBody)
			   | (_, _) =>
				 ((label, n, conArgs,
				   vpBody (body, env, isToplevel, shared))::
				  tagBodyList, elseBody)
		     end) (nil, elseBody) tagBodyVec
	    in
		(case tagBodyList of
		     nil => NONE
		   | _::_ => SOME (TagTests (Vector.fromList tagBodyList)),
		 elseBody)
	    end
	  | vpTests (id, ConTests conBodyVec,
		     elseBody, env, isToplevel, shared) =
	    let
		val (conBodyList, elseBody) =
		    Vector.foldr
		    (fn ((con, conArgs, body), (conBodyList, elseBody)) =>
		     let
			 val con =
			     case con of
				 Con id =>
				     (case IdMap.lookupExistent (env, id) of
					  (ConVal (con as StaticCon _,
						   _), _) => con
					| (_, _) => Con (deref (id, env)))
			       | StaticCon _ => con
			 val env = IdMap.cloneTop env
			 val entry = IdMap.lookupExistent (env, id)
			 val entry' =
			     (case conArgs of
				  NONE => ConVal (con, NONE)
				| SOME args => ConAppVal (con, args),
			      isToplevel)
			 val _ = IdMap.insert (env, id, entry')
			 val _ = declareConArgs (env, conArgs, isToplevel)
		     in
			 case (conArgs, entry) of
			     (NONE, (ConVal (con', _), _)) =>
				 if conEq (con, con') then
				     (nil, vpBody (body, env,
						   isToplevel, shared))
				 else (nil, elseBody)
			   | (SOME args, (ConAppVal (con', args'), _)) =>
				 if conEq (con, con') then
				     (aliasArgs (args, args', env, isToplevel);
				      (nil, vpBody (body, env,
						    isToplevel, shared)))
				 else (nil, elseBody)
			   | (_, _) =>
				 ((con, conArgs,
				   vpBody (body, env, isToplevel, shared))::
				  conBodyList, elseBody)
		     end) (nil, elseBody) conBodyVec
	    in
		(case conBodyList of
		     nil => NONE
		   | _::_ => SOME (ConTests (Vector.fromList conBodyList)),
		 elseBody)
	    end
	  | vpTests (id, VecTests vecBodyVec,
		     elseBody, env, isToplevel, shared) =
	    let
		val (vecBodyList, elseBody) =
		    Vector.foldr
		    (fn ((idDefs, body), (vecBodyList, elseBody)) =>
		     let
			 val env = IdMap.cloneTop env
			 val entry = IdMap.lookupExistent (env, id)
			 val entry' = (VecVal idDefs, isToplevel)
			 val _ = IdMap.insert (env, id, entry')
			 val _ = declareArgs (env, TupArgs idDefs, isToplevel)
		     in
			 case entry of
			     (VecVal idDefs', _) =>
				 if Vector.length idDefs =
				    Vector.length idDefs'
				 then (nil, vpBody (body, env,
						    isToplevel, shared))
				 else (nil, elseBody)
			   | (_, _) =>
				 ((idDefs,
				   vpBody (body, env, isToplevel, shared))::
				  vecBodyList, elseBody)
		     end) (nil, elseBody) vecBodyVec
	    in
		(case vecBodyList of
		     nil => NONE
		   | _::_ => SOME (VecTests (Vector.fromList vecBodyList)),
		 elseBody)
	    end
	and vpExp (exp as LitExp (_, _), _, _, _) = exp
	  | vpExp (exp as PrimExp (info, name), _, _, _) =
	    (PrimOps.getArity name
	     handle PrimOps.UnknownPrim =>
		 Error.error (#region info, "unknown primitive " ^ name);
	     exp)
	  | vpExp (exp as NewExp _, _, _, _) = exp
	    (*--** generate StaticCon if on toplevel *)
	  | vpExp (VarExp (info, id), env, _, _) = getTerm (info, id, env)
	  | vpExp (exp as TagExp (_, _, _), _, _, _) = exp
	  | vpExp (ConExp (info, con as Con id), env, _, _) =
	    let
		val id = deref (id, env)
	    in
		case IdMap.lookupExistent (env, id) of
		    (ConVal (con as StaticCon _, NONE), _) =>
			ConExp (info, con)
		  | (_, _) => ConExp (info, Con id)
	    end
	  | vpExp (exp as ConExp (_, StaticCon _), _, _, _) = exp
	  | vpExp (TupExp (info, ids), env, _, _) =
	    (*--** if TupExp took terms instead of ids -> getTerm *)
	    TupExp (info, Vector.map (fn id => deref (id, env)) ids)
	  | vpExp (ProdExp (info, labelIdVec), env, _, _) =
	    (*--** if ProdExp took terms instead of ids -> getTerm *)
	    ProdExp (info, Vector.map (fn (label, id) =>
				       (label, deref (id, env))) labelIdVec)
	  | vpExp (VecExp (info, ids), env, _, _) =
	    (*--** if VecExp took terms instead of ids -> getTerm *)
	    VecExp (info, Vector.map (fn id => deref (id, env)) ids)
	  | vpExp (FunExp (info, stamp, flags, args, body), env, _, _) =
	    (*--** do eta-conversion for TagAppExp ConAppExp RefAppExp SelAppExp *)
	    let
		val _ = IdMap.insertScope env
		val _ = declareArgs (env, args, false)
		val body = vpBodyShared (body, stamp, env, false)
		val _ = IdMap.deleteScope env
	    in
		FunExp (info, stamp, flags, args, body)
	    end
	  | vpExp (PrimAppExp (info, name, ids), env, _, _) =
	    vpPrimApp (info, name, Vector.map (fn id => deref (id, env)) ids)
	  | vpExp (VarAppExp (info, id, args), env, _, _) =
	    let
		val id = deref (id, env)
		val args = derefArgs (args, env)
	    in
		case IdMap.lookupExistent (env, id) of
		    (PrimVal name, _) =>
			primAppExp (info, deref (id, env), name,
				    valOf (PrimOps.getArity name), args, env)
		  | (TagVal (label, n, conArity), _) =>
			if arityMatches (args, conArity) then
			    TagAppExp (info, label, n, args)
			else VarAppExp (info, id, args)
		  | (ConVal (con, conArity), _) =>
			if arityMatches (args, conArity) then
			    ConAppExp (info, con, args)
			else VarAppExp (info, id, args)
		  | (RefVal, _) =>
			(case args of
			     OneArg id => RefAppExp (info, id)
			   | _ => VarAppExp (info, id, args))   (*--** *)
		  | (SelVal (label, n), _) =>
			(case derefArgs (args, env) of
			     OneArg id =>
				 doSel (info, label, n, id, env)
			   | TupArgs ids =>
				 VarExp (info, Vector.sub (ids, n))
			   | ProdArgs labelIdVec =>
				 VarExp (info,
					 #2 (Vector.sub (labelIdVec, n))))
		  | (FunVal (stamp, _), true) =>
			(*--** optimize args conversion *)
			FunAppExp (info, id, stamp, args)
		  | (_, _) => VarAppExp (info, id, args)
	    end
	  | vpExp (TagAppExp (info, label, n, args), env, _, _) =
	    TagAppExp (info, label, n, derefArgs (args, env))
	  | vpExp (ConAppExp (info, Con id, args), env, _, _) =
	    let
		val id = deref (id, env)
		val args = derefArgs (args, env)
	    in
		case IdMap.lookupExistent (env, id) of
		    (ConVal (con as StaticCon _, _), _) =>
			ConAppExp (info, con, args)
		  | (_, _) => ConAppExp (info, Con id, args)
	    end
	  | vpExp (ConAppExp (info, StaticCon stamp, args), env, _, _) =
	    ConAppExp (info, StaticCon stamp, derefArgs (args, env))
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

	fun debug component =
	    let
		val q = TextIO.openOut "vpdebug.txt"
	    in
		TextIO.output
		(q, "\n" ^ OutputFlatGrammar.outputComponent component ^ "\n");
		TextIO.closeOut q
	    end

	fun idToString (Id (_, stamp, Name.InId)) =
	    "$" ^ Stamp.toString stamp
	  | idToString (Id (_, stamp, Name.ExId s)) =
	    s ^ "$" ^ Stamp.toString stamp

	fun translate () (_, component as (imports, (body, sign))) =
	    let
		val env = IdMap.new ()
		val _ =
		    Vector.app (fn (idDef, _, _) =>
				declareUnknown (env, idDef, true)) imports
		val topStamp = Stamp.new ()
		val body' = vpBodyShared (body, topStamp, env, true)
		val component' = (imports, (body', sign))
	    in
		component'
	    end
	    (*--**DEBUG*)
	    handle exn as Error.Error (_, _) => raise exn
		 | exn =>
		       (TextIO.print
			"\nValuePropagationPhase crashed: \
			 \dumping debug information to vpdebug.txt\n";
			debug component;
			case exn of
			    IdMap.Lookup id =>
				TextIO.print ("Lookup " ^ idToString id ^ "\n")
			  | _ => ();
			raise exn)
    end
