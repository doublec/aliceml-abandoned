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

	fun killSet body =
	    case !(#liveness (infoStm (List.hd body))) of
		Kill set => set
	      | _ => raise Crash.Crash "DeadCodeEliminationPhase.killSet"

	fun elim (info: stm_info, body) =
	    case !(#liveness info) of
		Kill fromSet =>
		    let
			val toSet = killSet body
		    in
			StampSet.app (fn stamp =>
				      StampSet.insert (toSet, stamp)) fromSet
		    end
	      | _ => ()

	fun killIdDef (idDef as IdDef (Id (_, stamp, _)), set) =
	    if StampSet.member (set, stamp) then
		(StampSet.delete (set, stamp); Wildcard)
	    else idDef
	  | killIdDef (Wildcard, _) = Wildcard

	fun killArgs (OneArg idDef, set) = OneArg (killIdDef (idDef, set))
	  | killArgs (TupArgs idDefs, set) =
	    TupArgs (Vector.map (fn idDef => killIdDef (idDef, set)) idDefs)
	  | killArgs (ProdArgs labelIdDefVec, set) =
	    ProdArgs (Vector.map (fn (label, idDef) =>
				  (label, killIdDef (idDef, set)))
		      labelIdDefVec)

	fun killConArgs (SOME args, set) = SOME (killArgs (args, set))
	  | killConArgs (NONE, _) = NONE

	fun deadExp (LitExp (_, _)) = NONE
	  | deadExp (PrimExp (_, _)) = NONE
	  | deadExp (NewExp _) = NONE
	  | deadExp (VarExp (_, _)) = NONE
	  | deadExp (TagExp (_, _, _)) = NONE
	  | deadExp (TupExp (_, _)) = NONE
	  | deadExp (ProdExp (_, _)) = NONE
	  | deadExp (VecExp (_, _)) = NONE
	  | deadExp (FunExp (_, _, _, _, _)) = NONE
	  | deadExp (RefAppExp (_, _)) = NONE
	  | deadExp exp = SOME exp

	fun liveBody (ValDec (info, idDef, exp)::rest) =
	    let
		val rest = liveBody rest
	    in
		case killIdDef (idDef, killSet rest) of
		    idDef as IdDef _ =>
			ValDec (info, idDef, liveExp exp)::rest
		  | Wildcard =>
			(case deadExp exp of
			     SOME exp =>
				 ValDec (info, Wildcard, exp)::rest
			   | NONE => (elim (info, rest); rest))
	    end
	  | liveBody (RefAppDec (info, idDef, id)::rest) =
	    let
		val rest = liveBody rest
	    in
		RefAppDec (info, killIdDef (idDef, killSet rest), id)::rest
	    end
	  | liveBody (TupDec (info, idDefs, id)::rest) =
	    let
		val rest = liveBody rest
		val set = killSet rest
		val idDefs =
		    Vector.map (fn idDef => killIdDef (idDef, set)) idDefs
	    in
		TupDec (info, idDefs, id)::rest
	    end
	  | liveBody (ProdDec (info, labelIdDefVec, id)::rest) =
	    let
		val rest = liveBody rest
		val set = killSet rest
		val labelIdDefVec =
		    Vector.map (fn (label, idDef) =>
				(label, killIdDef (idDef, set))) labelIdDefVec
	    in
		ProdDec (info, labelIdDefVec, id)::rest
	    end
	  | liveBody (body as [RaiseStm (_, _)]) = body
	  | liveBody (body as [ReraiseStm (_, _)]) = body
	  | liveBody [TryStm (info, tryBody, idDef, handleBody)] =
	    (case liveBody tryBody of
		 [EndTryStm (_, body)] => body
	       | tryBody =>
		     let
			 val handleBody = liveBody handleBody
			 val idDef = killIdDef (idDef, killSet handleBody)
		     in
			 [TryStm (info, tryBody, idDef, handleBody)]
		     end)
	  | liveBody [EndTryStm (info, body)] =
	    [EndTryStm (info, liveBody body)]
	  | liveBody [EndHandleStm (info, body)] =
	    [EndHandleStm (info, liveBody body)]
	  | liveBody [TestStm (info, id, tests, body)] =
	    let
		val tests =
		    case tests of
			LitTests tests =>
			    LitTests (Vector.map
				      (fn (lit, body) => (lit, liveBody body))
				      tests)
		      | TagTests tests =>
			    TagTests (Vector.map
				      (fn (label, tag, conArgs, body) =>
				       let
					   val body = liveBody body
					   val set = killSet body
					   val conArgs =
					       killConArgs (conArgs, set)
				       in
					   (label, tag, conArgs, body)
				       end) tests)
		      | ConTests tests =>
			    ConTests (Vector.map
				      (fn (con, conArgs, body) =>
				       let
					   val body = liveBody body
					   val set = killSet body
					   val conArgs =
					       killConArgs (conArgs, set)
				       in
					   (con, conArgs, body)
				       end) tests)
		      | VecTests tests =>
			    VecTests (Vector.map
				      (fn (idDefs, body) =>
				       let
					   val body = liveBody body
					   val set = killSet body
				       in
					   (Vector.map (fn idDef =>
							killIdDef (idDef, set))
					    idDefs, body)
				       end) tests)
		val body = liveBody body
	    in
		[TestStm (info, id, tests, body)]
	    end
	  | liveBody [SharedStm (info, body, stamp)] =
	    [SharedStm (info, liveBody body, stamp)]
	  | liveBody [ReturnStm (info, exp)] =
	    [ReturnStm (info, liveExp exp)]
	  | liveBody [IndirectStm (info, ref (SOME body))] =
	    (elim (info, body); liveBody body)
	  | liveBody [IndirectStm (_, ref NONE)] =
	    raise Crash.Crash "DeadCodeEliminationPhase.liveBody 1"
	  | liveBody [ExportStm (info, exp)] =
	    [ExportStm (info, liveExp exp)]
	  | liveBody ((RaiseStm (_, _) | ReraiseStm (_, _) |
		       TryStm (_, _, _, _) | EndTryStm (_, _) |
		       EndHandleStm (_, _) | TestStm (_, _, _, _) |
		       SharedStm (_, _, _) | ReturnStm (_, _) |
		       IndirectStm (_, _) | ExportStm (_, _))::_::_ | nil) =
	    raise Crash.Crash "DeadCodeEliminationPhase.liveBody 2"
	and liveExp (FunExp (info, stamp, funFlags, args, body)) =
	    let
		val body = liveBody body
	    in
		FunExp (info, stamp, funFlags, killArgs (args, killSet body),
			body)
	    end
	  | liveExp exp = exp

	fun translate () (_, component as (imports, (body, sign))) =
	    (imports, (liveBody body, sign))
    end
