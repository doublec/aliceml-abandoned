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

structure CodeGenPhase: CODE_GEN_PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = PickleGrammar

	open I
	open Environment

	fun translateLit (IntLit i) = O.Int i
	  | translateLit (WordLit w) = O.Int (LargeWord.toLargeInt w)
	  | translateLit (CharLit c) = O.Int (LargeInt.fromInt (Char.ord c))
	  | translateLit (StringLit s) = O.String s
	  | translateLit (RealLit r) = O.Real r

	fun litToInt (IntLit i) = i
	  | litToInt (WordLit w) = LargeWord.toLargeInt w
	  | litToInt (CharLit c) = LargeInt.fromInt (Char.ord c)
	  | litToInt (StringLit _ | RealLit _) =
	    raise Crash.Crash "CodeGenPhase.litToInt"

	fun translateId (id, env) = lookup (env, id)

	fun translateIds (ids, env) =
	    Vector.map (fn id => lookup (env, id)) ids

	fun translateIdDef (IdDef id, env) = O.IdDef (declare (env, id))
	  | translateIdDef (Wildcard, _) = O.Wildcard

	fun translateCon (Con id, env) = O.Con (lookup (env, id))
	  | translateCon (StaticCon stamp, _) =
	    O.StaticCon (O.Constructor stamp)

	fun translateArgs f (OneArg idDef, env) =
	    O.OneArg (f (idDef, env))
	  | translateArgs f (TupArgs idDefs, env) =
	    O.TupArgs (Vector.map (fn idDef => f (idDef, env)) idDefs)
	  | translateArgs f (ProdArgs labelIdDefList, env) =
	    translateArgs f (TupArgs (Vector.map #2 labelIdDefList), env)

	fun argsToVector (OneArg x) = #[x]
	  | argsToVector (TupArgs xs) = xs
	  | argsToVector (ProdArgs labelXList) = Vector.map #2 labelXList

	fun translateConArgs (conArgs, env) =
	    Vector.map (fn idDef => translateIdDef (idDef, env))
	    (argsToVector (valOf conArgs))

	fun isId (O.Local i, id) = i = id
	  | isId (O.Global _, _) = false

	fun isIdInVec (idRefs, id) =
	    Vector.exists (fn idRef => isId (idRef, id)) idRefs

	fun isIdInArgs (O.OneArg idRef, id) = isId (idRef, id)
	  | isIdInArgs (O.TupArgs idRefs, id) = isIdInVec (idRefs, id)

	fun getLocal (O.Local i) = SOME i
	  | getLocal (O.Global _) = NONE

	(*
	 * Function `detup' tries to find out whether the value referenced
	 * by a given identifier can actually be represented `flat', i.e.,
	 * by storing tuples in deconstructed form (component-wise).
	 *
	 * This is allowed if, in the instruction stream, we can see
	 * a deconstruction (GetTup) that will definitely be performed,
	 * and this before any side-effect - else we might corrupt the
	 * order of side-effects.  Furthermore, it is checked that the
	 * (constructed) value is not used again after this point.
	 *
	 * The implementation computes an approximation.
	 *)

	datatype detup_result =
	    USED
	  | SIDE_EFFECTING
	  | UNKNOWN
	  | KILLED
	  | DECONSTRUCTED of O.idDef vector * O.instr

	fun detup' (DECONSTRUCTED (idDefs, instr), f) =
	    DECONSTRUCTED (idDefs, f instr)
	  | detup' (result, _) = result

	fun detup (O.Kill (ids, instr), id) =
	    if Vector.exists (fn id' => id = id') ids then KILLED
	    else detup' (detup (instr, id), fn instr => O.Kill (ids, instr))
	  | detup (O.PutConst (id, value, instr), id') =
	    detup' (detup (instr, id'),
		    fn instr => O.PutConst (id, value, instr))
	  | detup (O.PutVar (id, idRef, instr), id') =
	    if isId (idRef, id') then USED
	    else detup' (detup (instr, id'),
			 fn instr => O.PutVar (id, idRef, instr))
	  | detup (O.PutNew (id, instr), id') =
	    detup' (detup (instr, id'), fn instr => O.PutNew (id, instr))
	  | detup (O.PutTag (id, tag, idRefs, instr), id') =
	    if isIdInVec (idRefs, id') then USED
	    else detup' (detup (instr, id'),
			 fn instr => O.PutTag (id, tag, idRefs, instr))
	  | detup (O.PutCon (_, _, _, _), _) = SIDE_EFFECTING
	  | detup (O.PutRef (id, idRef, instr), id') =
	    if isId (idRef, id') then USED
	    else detup' (detup (instr, id'),
			 fn instr => O.PutRef (id, idRef, instr))
	  | detup (O.PutTup (id, idRefs, instr), id') =
	    if isIdInVec (idRefs, id') then USED
	    else detup' (detup (instr, id'),
			 fn instr => O.PutTup (id, idRefs, instr))
	  | detup (O.PutVec (id, idRefs, instr), id') =
	    if isIdInVec (idRefs, id') then USED
	    else detup' (detup (instr, id'),
			 fn instr => O.PutVec (id, idRefs, instr))
	  | detup (O.PutFun (id, idRefs, function, instr), id') =
	    if isIdInVec (idRefs, id') then USED
	    else detup' (detup (instr, id'),
			 fn instr => O.PutFun (id, idRefs, function, instr))
	  | detup (O.AppPrim (_, _, _), _) = SIDE_EFFECTING
	  | detup (O.AppVar (_, _, _), _) = SIDE_EFFECTING
	  | detup (O.AppConst (_, _, _), _) = SIDE_EFFECTING
	  | detup (O.GetRef (_, _, _), _) = SIDE_EFFECTING
	  | detup (O.GetTup (idDefs, idRef, instr), id) =
	    if isId (idRef, id) then
		case detup (instr, id) of
		    DECONSTRUCTED (_, _) => USED
		  | KILLED => DECONSTRUCTED (idDefs, instr)
		  | res as (USED | SIDE_EFFECTING | UNKNOWN) => res
	    else detup' (detup (instr, id),
			 fn instr => O.GetTup (idDefs, idRef, instr))
	  | detup (O.Raise idRef, id) =
	    if isId (idRef, id) then USED else KILLED
	  | detup (O.Try (_, _, _), _) = UNKNOWN
	  | detup (O.EndTry _, _) = UNKNOWN
	  | detup (O.EndHandle _, _) = UNKNOWN
	  | detup (O.IntTest (_, _, _), _) = UNKNOWN
	  | detup (O.RealTest (_, _, _), _) = UNKNOWN
	  | detup (O.StringTest (_, _, _), _) = UNKNOWN
	  | detup (O.WideStringTest (_, _, _), _) = UNKNOWN
	  | detup (O.TagTest (_, _, _, _), _) = UNKNOWN
	  | detup (O.ConTest (_, _, _, _), _) = UNKNOWN
	  | detup (O.VecTest (_, _, _), _) = UNKNOWN
	  | detup (O.Shared (_, _), _) = UNKNOWN
	  | detup (O.Return idRefArgs, id) =
	    if isIdInArgs (idRefArgs, id) then USED else KILLED

	fun translateBody (stm::stms, env) =
	    let
		val instr =
		    case stms of
			_::_ =>
			    let
				val _ = doDec (stm, env)
				val instr = translateBody (stms, env)
			    in
				translateDec (stm, instr, env)
			    end
		      | nil => translateStm (stm, env)
	    in
		case #liveness (infoStm stm) of
		    ref (Kill set) =>
			let
			    val idRefs =
				StampSet.fold (fn (stamp, rest) =>
					       case lookupStamp (env, stamp) of
						   SOME idRef => idRef::rest
						 | NONE => rest) nil set
			    val ids = List.mapPartial getLocal idRefs
			in
			    O.Kill (Vector.fromList ids, instr)
			end
		  | ref _ => instr
	    end
	  | translateBody (nil, _) =
	    raise Crash.Crash "CodeGenPhase.translateBody"
	and doDec (ValDec (_, IdDef id, _), env) = ignore (declare (env, id))
	  | doDec (ValDec (_, Wildcard, _), _) = ()
	  | doDec (RecDec (_, idDefExpVec), env) =
	    Vector.app (fn (idDef, _) =>
			case idDef of
			    IdDef id => ignore (declare (env, id))
			  | Wildcard => ()) idDefExpVec
	  | doDec (RefAppDec (_, IdDef id, _), env) =
	    ignore (declare (env, id))
	  | doDec (RefAppDec (_, Wildcard, _), _) = ()
	  | doDec (TupDec (_, idDefs, _), env) =
	    Vector.app (fn idDef =>
			case idDef of
			    IdDef id => ignore (declare (env, id))
			  | Wildcard => ()) idDefs
	  | doDec (ProdDec (info, labelIdDefVec, id), env) =
	    doDec (TupDec (info, Vector.map #2 labelIdDefVec, id), env)
	  | doDec ((RaiseStm (_, _) | ReraiseStm (_, _) |
		    TryStm (_, _, _, _) | EndTryStm (_, _) |
		    EndHandleStm (_, _) | TestStm (_, _, _, _) |
		    SharedStm (_, _, _) | ReturnStm (_, _) |
		    IndirectStm (_, _) | ExportStm (_, _)), _) =
	    raise Crash.Crash "CodeGenPhase.doDec"
	and translateDec (ValDec (_, IdDef id, exp), instr, env) =
	    translateExp (exp, declare (env, id), instr, env)
	  | translateDec (ValDec (_, Wildcard, exp), instr, env) =
	    translateIgnore (exp, instr, env)
	  | translateDec (RecDec (_, idDefExpVec), instr, env) =
	    let
		val ids =
		    Vector.foldr (fn ((idDef, _), ids) =>
				  case idDef of
				      IdDef id => declare (env, id)::ids
				    | Wildcard => ids) nil idDefExpVec
		val bodiesInstr =
		    Vector.foldr
		    (fn ((idDef, exp), instr) =>
		     case idDef of
			 IdDef id =>
			     let
				 val id' = fresh env
				 val instr' =
				     O.AppPrim ("Hole.fill",
						#[O.Local id',
						  lookup (env, id)],
						SOME (O.Wildcard, instr))
			     in
				 translateExp (exp, id', instr', env)
			     end
		       | Wildcard => translateIgnore (exp, instr, env))
		    instr idDefExpVec
	    in
		List.foldr (fn (id, instr) =>
			    O.AppPrim ("Hole.hole", #[],
				       SOME (O.IdDef id, instr)))
		bodiesInstr ids
	    end
	  | translateDec (RefAppDec (_, IdDef id, id'), instr, env) =
	    O.GetRef (declare (env, id), translateId (id', env), instr)
	  | translateDec (RefAppDec (_, Wildcard, id), instr, env) =
	    let
		val id' = lookup (env, id)
	    in
		O.AppPrim ("Future.await", #[id'], SOME (O.Wildcard, instr))
	    end
	  | translateDec (TupDec (_, idDefs, id), instr, env) =
	    let
		fun f id = translateIdDef (id, env)
	    in
		O.GetTup (Vector.map f idDefs, lookup (env, id), instr)
	    end
	  | translateDec (ProdDec (info, labelIdDefVec, id), instr, env) =
	    translateDec (TupDec (info, Vector.map #2 labelIdDefVec, id),
			  instr, env)
	  | translateDec ((RaiseStm (_, _) | ReraiseStm (_, _) |
			   TryStm (_, _, _, _) | EndTryStm (_, _) |
			   EndHandleStm (_, _) | TestStm (_, _, _, _) |
			   SharedStm (_, _, _) | ReturnStm (_, _) |
			   IndirectStm (_, _) | ExportStm (_, _)), _, _) =
	    raise Crash.Crash "CodeGenPhase.translateDec"
	and translateStm ((ValDec (_, _, _) | RecDec (_, _) |
			   RefAppDec (_, _, _) |
			   TupDec (_, _, _) | ProdDec (_, _, _)), _) =
	    raise Crash.Crash "CodeGenPhase.translateStm"
	  | translateStm (RaiseStm (_, id), env) =
	    O.Raise (lookup (env, id))
	  | translateStm (ReraiseStm (_, id), env) = (*--** do better *)
	    O.Raise (lookup (env, id))
	  | translateStm (TryStm (_, tryBody, idDef, handleBody), env) =
	    O.Try (translateBody (tryBody, env),
		   translateIdDef (idDef, env),
		   translateBody (handleBody, env))
	  | translateStm (EndTryStm (_, body), env) =
	    O.EndTry (translateBody (body, env))
	  | translateStm (EndHandleStm (_, body), env) =
	    O.EndHandle (translateBody (body, env))
	  | translateStm (TestStm (_, id, LitTests #[], elseBody),
			  env) =
	    translateBody (elseBody, env)
	  | translateStm (TestStm (_, id, LitTests litTests, elseBody), env) =
	    (case Vector.sub (litTests, 0) of
		 ((IntLit _ | WordLit _ | CharLit _), _) =>
		     O.IntTest
		     (lookup (env, id),
		      Vector.map (fn (lit, body) =>
				  (litToInt lit, translateBody (body, env)))
		      litTests,
		      translateBody (elseBody, env))
	       | (StringLit _, _) =>
		     O.StringTest
		     (lookup (env, id),
		      Vector.map (fn (lit, body) =>
				  case lit of StringLit s =>
				      (s, translateBody (body, env))
				    | _ => raise Match) litTests,
		      translateBody (elseBody, env))
	       | (RealLit _, _) =>
		     O.RealTest
		     (lookup (env, id),
		      Vector.map (fn (lit, body) =>
				  case lit of RealLit r =>
				      (r, translateBody (body, env))
				    | _ => raise Match) litTests,
		      translateBody (elseBody, env)))
	  | translateStm (TestStm (_, id, TagTests tagTests, elseBody), env) =
	    let
		val (naryTests, nullaryTests) =
		    List.partition (fn (_, _, conArgs, _) => isSome conArgs)
		    (Vector.toList tagTests)
	    in
		O.TagTest (lookup (env, id),
			   Vector.map (fn (_, tag, _, body) =>
				       (tag, translateBody (body, env)))
			   (Vector.fromList nullaryTests),
			   Vector.map (fn (_, tag, conArgs, body) =>
				       (tag, translateConArgs (conArgs, env),
					translateBody (body, env)))
			   (Vector.fromList naryTests),
			   translateBody (elseBody, env))
	    end
	  | translateStm (TestStm (_, id, ConTests conTests, elseBody), env) =
	    let
		val (naryTests, nullaryTests) =
		    List.partition (fn (_, conArgs, _) => isSome conArgs)
		    (Vector.toList conTests)
	    in
		O.ConTest (lookup (env, id),
			   Vector.map (fn (con, _, body) =>
				       (translateCon (con, env),
					translateBody (body, env)))
			   (Vector.fromList nullaryTests),
			   Vector.map (fn (con, conArgs, body) =>
				       (translateCon (con, env),
					translateConArgs (conArgs, env),
					translateBody (body, env)))
			   (Vector.fromList naryTests),
			   translateBody (elseBody, env))
	    end
	  | translateStm (TestStm (_, id, VecTests vecTests, elseBody), env) =
	    O.VecTest (lookup (env, id),
		       Vector.map (fn (idDefs, body) =>
				   (Vector.map (fn idDef =>
						translateIdDef (idDef, env))
				    idDefs,
				    translateBody (body, env)))
		       vecTests,
		       translateBody (elseBody, env))
	  | translateStm (SharedStm (_, body, stamp), env) =
	    (case lookupShared (env, stamp) of
		 SOME instr => instr
	       | NONE =>
		     let
			 val instr =
			     O.Shared (stamp, translateBody (body, env))
		     in
			 declareShared (env, stamp, instr); instr
		     end)
	  | translateStm (ReturnStm (_, TupExp (_, ids)), env) =
	    O.Return (O.TupArgs (translateIds (ids, env)))
	  | translateStm (ReturnStm (_, ProdExp (_, labelIdVec)), env) =
	    O.Return (O.TupArgs (Vector.map (fn (_, id) => lookup (env, id))
				 labelIdVec))
	  | translateStm (ReturnStm (_, PrimAppExp (_, name, ids)), env) =
	    O.AppPrim (name, translateIds (ids, env), NONE)
	  | translateStm (ReturnStm (_, VarAppExp (_, id, args)), env) =
	    O.AppVar (lookup (env, id),
		      translateArgs translateId (args, env), NONE)
	  | translateStm (ReturnStm (_, FunAppExp (_, id, _, args)), env) =
	    (*--** translate to AppConst *)
	    O.AppVar (lookup (env, id),
		      translateArgs translateId (args, env), NONE)
	  | translateStm (ReturnStm (_, exp), env) =
	    let
		val id = fresh env
	    in
		translateExp (exp, id, O.Return (O.OneArg (O.Local id)), env)
	    end
	  | translateStm (IndirectStm (_, ref bodyOpt), env) =
	    translateBody (valOf bodyOpt, env)
	  | translateStm (ExportStm (info, exp), env) =
	    translateStm (ReturnStm (info, exp), env)
	and translateExp (LitExp (_, lit), id, instr, _) =
	    O.PutConst (id, translateLit lit, instr)
	  | translateExp (PrimExp (_, name), id, instr, _) =
	    O.PutConst (id, O.Prim name, instr)
	  | translateExp (NewExp _, id, instr, _) = O.PutNew (id, instr)
	  | translateExp (VarExp (_, id'), id, instr, env) =
	    O.PutVar (id, lookup (env, id'), instr)
	  | translateExp (TagExp (_, _, tag), id, instr, _) =
	    O.PutConst (id, O.Int (LargeInt.fromInt tag), instr)
	  | translateExp (ConExp (_, Con id'), id, instr, env) =
	    O.PutVar (id, lookup (env, id'), instr)
	  | translateExp (ConExp (_, StaticCon s), id, instr, _) =
	    O.PutConst (id, O.Constructor s, instr)
	  | translateExp (TupExp (_, ids), id, instr, env) =
	    O.PutTup (id, translateIds (ids, env), instr)
	  | translateExp (ProdExp (info, labelIdVec), id, instr, env) =
	    translateExp (TupExp (info, Vector.map #2 labelIdVec),
			  id, instr, env)
	  | translateExp (VecExp (_, ids), id, instr, env) =
	    O.PutVec (id, translateIds (ids, env), instr)
	  | translateExp (FunExp (_, _, _, args, body), id, instr, env) =
	    let
		val _ = startFn env
		val args' = translateArgs translateIdDef (args, env)
		val bodyInstr = translateBody (body, env)
		val (globals, nlocals) = endFn env
		val function =
		    O.Function (Vector.length globals, nlocals,
				args', bodyInstr)
	    in
		O.PutFun (id, translateIds (globals, env), function, instr)
	    end
	  | translateExp (PrimAppExp (_, name, ids), id, instr, env) =
	    O.AppPrim (name, translateIds (ids, env), SOME (O.IdDef id, instr))
	  | translateExp (VarAppExp (_, id, args), id', instr, env) =
	    let
		val (returnArgs, instr) =
		    case detup (instr, id') of
			DECONSTRUCTED (idDefs, instr) =>
			    (O.TupArgs idDefs, instr)
		      | (USED | SIDE_EFFECTING | UNKNOWN) =>
			    (O.OneArg (O.IdDef id'), instr)
		      | KILLED => (O.OneArg O.Wildcard, instr)
	    in
		O.AppVar (lookup (env, id),
			  translateArgs translateId (args, env),
			  SOME (returnArgs, instr))
	    end
	  | translateExp (TagAppExp (_, _, tag, args), id, instr, env) =
	    O.PutTag (id, tag, translateIds (argsToVector args, env), instr)
	  | translateExp (ConAppExp (_, con, args), id, instr, env) =
	    O.PutCon (id, translateCon (con, env),
		      translateIds (argsToVector args, env), instr)
	  | translateExp (RefAppExp (_, id), id', instr, env) =
	    O.PutRef (id', lookup (env, id), instr)
	  | translateExp (SelAppExp (_, prod, _, index, id), id', instr, env) =
	    let
		val n =
		    case prod of
			Tuple n => n
		      | Product labels => Vector.length labels
		fun wild _ = O.Wildcard
	    in
		O.GetTup (Vector.fromList
			  (List.tabulate (index, wild) @
			   O.IdDef id'::List.tabulate (n - index - 1, wild)),
			  lookup (env, id), instr)
	    end
	  | translateExp (FunAppExp (info, id, _, args), id', instr, env) =
	    (*--** translate to AppConst *)
	    translateExp (VarAppExp (info, id, args), id', instr, env)
	and translateIgnore (LitExp (_, _), instr, _) = instr
	  | translateIgnore (PrimExp (_, _), instr, _) = instr
	  | translateIgnore (NewExp _, instr, _) = instr
	  | translateIgnore (VarExp (_, _), instr, _) = instr
	  | translateIgnore (TagExp (_, _, _), instr, _) = instr
	  | translateIgnore (ConExp (_, _), instr, _) = instr
	  | translateIgnore (TupExp (_, _), instr, _) = instr
	  | translateIgnore (ProdExp (_, _), instr, _) = instr
	  | translateIgnore (VecExp (_, _), instr, _) = instr
	  | translateIgnore (FunExp (_, _, _, _, _), instr, _) = instr
	  | translateIgnore (PrimAppExp (_, name, ids), instr, env) =
	    O.AppPrim (name, translateIds (ids, env), SOME (O.Wildcard, instr))
	  | translateIgnore (VarAppExp (_, id, args), instr, env) =
	    O.AppVar (lookup (env, id), translateArgs translateId (args, env),
		      SOME (O.OneArg O.Wildcard, instr))
	  | translateIgnore (TagAppExp (_, _, _, _), instr, _) = instr
	  | translateIgnore (ConAppExp (_, Con id, _), instr, env) =
	    O.AppPrim ("Future.await", #[lookup (env, id)],
		       SOME (O.Wildcard, instr))
	  | translateIgnore (ConAppExp (_, StaticCon _, _), instr, _) = instr
	  | translateIgnore (RefAppExp (_, _), instr, _) = instr
	  | translateIgnore (SelAppExp (_, _, _, _, id), instr, env) =
	    O.AppPrim ("Future.await", #[lookup (env, id)],
		       SOME (O.Wildcard, instr))
	  | translateIgnore (FunAppExp (info, id, _, args), instr, env) =
	    (*--** translate to AppConst *)
	    translateIgnore (VarAppExp (info, id, args), instr, env)

	fun translate () (desc, (imports, (body, exportSign))) =
	    let
		val imports' =
		    Vector.map (fn (_, sign, url) =>
				O.Tuple #[O.Sign sign,
					  O.String (Url.toString url)]) imports
		val env = new ()
		val _ = startFn env
		val formalId = fresh env
		val idDefs =
		    Vector.map (fn (idDef, _, _) =>
				translateIdDef (idDef, env)) imports
		val instr = translateBody (body, env)
		val raiseId = fresh env
		val (globals, nlocals) = endFn env
		val _ = Assert.assert (Vector.length globals = 0)
		val elseInstr =
		    O.PutConst (raiseId, O.Prim "General.Match",
				O.Raise (O.Local raiseId))
		val bodyInstr =
		    O.VecTest (O.Local formalId, #[(idDefs, instr)], elseInstr)
		val function =
		    O.Function (Vector.length globals, nlocals,
				O.OneArg (O.IdDef formalId), bodyInstr)
	    in
		O.Tuple #[O.Vector imports',
			  O.Closure (function, #[]),
			  O.Sign exportSign]
	    end
    end
