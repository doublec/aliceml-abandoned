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

structure MatchCompilationPhase :> MATCH_COMPILATION_PHASE =
    struct
	structure I = IntermediateGrammar
	structure O = ImperativeGrammar

	open I
	open IntermediateAux
	open SimplifyMatch
	val region = IntermediateInfo.region

	(*--** provide types for the following six ids: *)
	val id_false = Id ((Source.nowhere, NONE),
			   Prebound.stamp_false, Name.ExId "false")
	val id_true = Id ((Source.nowhere, NONE),
			  Prebound.stamp_true, Name.ExId "true")
	val id_Match = Id ((Source.nowhere, NONE),
			   Prebound.stamp_Match, Name.ExId "Match")
	val id_Bind = Id ((Source.nowhere, NONE),
			  Prebound.stamp_Bind, Name.ExId "Bind")

	val longid_true = ShortId ((Source.nowhere, NONE), id_true)
	val longid_false = ShortId ((Source.nowhere, NONE), id_false)

	structure FieldLabelSort =
	    MakeLabelSort(type 'a t = Label.t * 'a
			  fun get (label, _) = label)

	type mapping = (pos * id) list

	fun lookup (pos, (pos', id)::mappingRest) =
	    if pos = pos' then id
	    else lookup (pos, mappingRest)
	  | lookup (pos, nil) =
	    raise Crash.Crash "MatchCompilationPhase.lookup"

	fun mappingsToSubst (mapping0, mapping) =
	    List.map (fn (pos, id) => (id, lookup (pos, mapping))) mapping0

	(* Translation *)

	fun stmInfo info = (region info, ref O.Unknown)

	fun share nil = nil
	  | share (stms as [O.SharedStm (_, _, _)]) = stms
	  | share stms =
	    [O.SharedStm (stmInfo (Source.nowhere, NONE), stms, ref 0)]

	datatype continuation =
	    Decs of dec list * continuation
	  | Goto of O.body
	  | Share of O.body option ref * continuation

	fun translateLongid (ShortId (_, id)) = (nil, id)
	  | translateLongid (LongId (info, longid, Lab (_, s))) =
	    let
		val (stms, id) = translateLongid longid
		val id' = freshId info
		val stm =
		    O.ValDec (stmInfo info, id',
			      O.SelAppExp (info, s, id), false)
	    in
		(stms @ [stm], id')
	    end

	fun decsToIdExpList (O.ValDec (_, id, exp', _)::rest, coord) =
	    (id, exp')::decsToIdExpList (rest, coord)
	  | decsToIdExpList (O.IndirectStm (_, ref (SOME body))::rest, coord) =
	    decsToIdExpList (body, coord) @ decsToIdExpList (rest, coord)
	  | decsToIdExpList (_::_, coord) =
	    Error.error (coord, "not admissible")
	  | decsToIdExpList (nil, _) = nil

	fun translateCont (Decs (dec::decr, cont)) =
	    translateDec (dec, Decs (decr, cont))
	  | translateCont (Decs (nil, cont)) = translateCont cont
	  | translateCont (Goto stms) = stms
	  | translateCont (Share (r as ref NONE, cont)) =
	    let
		val stms = share (translateCont cont)
	    in
		r := SOME stms; stms
	    end
	  | translateCont (Share (ref (SOME stms), _)) = stms
	and translateDec (ValDec (info, VarPat (_, id), exp), cont) =
	    let
		fun declare exp' = O.ValDec (stmInfo info, id, exp', false)
	    in
		translateExp (exp, declare, cont)
	    end
	  | translateDec (ValDec (info, pat, exp), cont) =
	    let
		val matches = [(region info, pat, translateCont cont)]
	    in
		simplifyCase (info, exp, matches, id_Bind, false)
	    end
	  | translateDec (RecDec (info, decs), cont) =
	    let
		val (constraints, idExpList, subst) = SimplifyRec.derec decs
		val aliasDecs =
		    List.map (fn (fromId, toId) =>
			      let
				  val info = infoId toId
				  val toExp = O.VarExp (info, toId)
			      in
				  O.ValDec (stmInfo (infoId fromId),
					    fromId, toExp, false)
			      end) subst
		val decs' =
		    List.foldr (fn ((id, exp), decs) =>
				translateExp (substExp (exp, subst),
					      fn exp' =>
					      O.ValDec (stmInfo (infoExp exp),
							id, exp', false),
					      Goto decs)) nil idExpList
		val idExpList' = decsToIdExpList (decs', region info)
		val rest =
		    O.RecDec (stmInfo info, idExpList', false)::aliasDecs @
		    translateCont cont
		val errStms = share [O.RaiseStm (stmInfo info, id_Bind)]
	    in
		List.foldr
		(fn ((longid1, longid2, hasArgs), rest) =>
		 let
		     val (stms1, id1) = translateLongid longid1
		     val (stms2, id2) = translateLongid longid2
		 in
		     stms1 @ stms2 @
		     [O.TestStm (stmInfo info, id1,
				 O.ConTest (id2, NONE), rest, errStms)]
		 end) rest constraints
	    end
	and unfoldTerm (VarExp (_, longid), cont) =
	    let
		val (stms, id) = translateLongid longid
	    in
		(stms @ translateCont cont, id)
	    end
	  | unfoldTerm (exp, cont) =
	    let
		val info = infoExp exp
		val id' = freshId info
		fun declare exp' = O.ValDec (stmInfo info, id', exp', false)
		val stms = translateExp (exp, declare, cont)
	    in
		(stms, id')
	    end
	and unfoldArgs (TupExp (_, exps), rest) =
	    let
		val (stms, ids) =
		    List.foldr (fn (exp, (stms, ids)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', id::ids)
				end) (rest, nil) exps
	    in
		(stms, O.TupArgs ids)
	    end
	  (*--** RecArgs *)
	  | unfoldArgs (exp, rest) =
	    let
		val (stms, id) = unfoldTerm (exp, Goto rest)
	    in
		(stms, O.OneArg id)
	    end
	and translateExp (LitExp (info, lit), f, cont) =
	    f (O.LitExp (info, lit))::translateCont cont
	  | translateExp (PrimExp (info, s), f, cont) =
	    f (O.PrimExp (info, s))::translateCont cont
	  | translateExp (NewExp (info, stringOpt, hasArgs), f, cont) =
	    f (O.NewExp (info, stringOpt, hasArgs))::translateCont cont
	  | translateExp (VarExp (info, longid), f, cont) =
	    let
		val (stms, id) = translateLongid longid
	    in
		stms @ f (O.VarExp (info, id))::translateCont cont
	    end
	  | translateExp (ConExp (info, longid, hasArgs), f, cont) =
	    let
		val (stms, id) = translateLongid longid
	    in
		stms @ f (O.ConExp (info, id, hasArgs))::translateCont cont
	    end
	  | translateExp (RefExp info, f, cont) =
	    f (O.RefExp info)::translateCont cont
	  | translateExp (TupExp (info, exps), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms, ids) =
		    List.foldr (fn (exp, (stms, ids)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', id::ids)
				end) (rest, nil) exps
	    in
		r := SOME (f (O.TupExp (info, ids))::translateCont cont);
		stms
	    end
	  | translateExp (RowExp (info, expFields), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms, fields) =
		    List.foldr (fn (Field (_, Lab (_, label), exp),
				    (stms, fields)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', (label, id)::fields)
				end) (rest, nil) expFields
		val exp' =
		    case FieldLabelSort.sort fields of
			(fields', FieldLabelSort.Tup _) =>
			    O.TupExp (info, List.map #2 fields')
		      | (fields', FieldLabelSort.Rec) =>
			    O.RecExp (info, fields')
	    in
		r := SOME (f exp'::translateCont cont);
		stms
	    end
	  | translateExp (SelExp (info, Lab (_, label)), f, cont) =
	    f (O.SelExp (info, label))::translateCont cont
	  | translateExp (VecExp (info, exps), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms, ids) =
		    List.foldr (fn (exp, (stms, ids)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', id::ids)
				end) (rest, nil) exps
	    in
		r := SOME (f (O.VecExp (info, ids))::translateCont cont);
		stms
	    end
	  | translateExp (FunExp (info, id, exp), f, cont) =
	    let
		fun return exp' = O.ReturnStm (stmInfo (infoExp exp), exp')
		val argsBodyList =
		    case exp of
			CaseExp (_, VarExp (_, ShortId (_, id')), matches) =>
			    if idEq (id, id')
				andalso not (occursInMatches (matches, id))
			    then translateFunBody (info, id, matches, return)
			    else
				[(O.OneArg id,
				  translateExp (exp, return, Goto nil))]
		      | _ =>
			    [(O.OneArg id,
			      translateExp (exp, return, Goto nil))]
	    in
		f (O.FunExp (info, Stamp.new (), nil, argsBodyList))::
		translateCont cont
	    end
	  | translateExp (AppExp (info, ConExp (_, longid, true), exp2),
			  f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms2, args) = unfoldArgs (exp2, rest)
		val (stms1, id1) = translateLongid longid
	    in
		r := SOME (f (O.ConAppExp (info, id1, args))::
			   translateCont cont);
		stms1 @ stms2
	    end
	  | translateExp (AppExp (info, RefExp _, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms2, args) = unfoldArgs (exp2, rest)
	    in
		(r := SOME (f (O.RefAppExp (info, args))::translateCont cont);
		 stms2)
	    end
	  | translateExp (AppExp (info, SelExp (_, Lab (_, s)), exp2),
			  f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms2, id2) = unfoldTerm (exp2, Goto rest)
	    in
		(r := SOME (f (O.SelAppExp (info, s, id2))::
			    translateCont cont);
		 stms2)
	    end
	  | translateExp (AppExp (info, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms2, args) = unfoldArgs (exp2, rest)
		val (stms1, id1) = unfoldTerm (exp1, Goto stms2)
	    in
		r := SOME (f (O.AppExp (info, id1, args))::
			   translateCont cont);
		stms1
	    end
	  | translateExp (AdjExp (info, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms2, id2) = unfoldTerm (exp2, Goto rest)
		val (stms1, id1) = unfoldTerm (exp1, Goto stms2)
	    in
		r := SOME (f (O.AdjExp (info, id1, id2))::translateCont cont);
		stms1
	    end
	  | translateExp (UpExp (_, exp), f, cont) =
	    translateExp (exp, f, cont)   (*--** UpExp *)
	  | translateExp (AndExp (info, exp1, exp2), f, cont) =
	    translateExp (IfExp (info, exp1,
				 exp2, VarExp (info, longid_false)), f, cont)
	  | translateExp (OrExp (info, exp1, exp2), f, cont) =
	    translateExp (IfExp (info, exp1,
				 VarExp (info, longid_true), exp2), f, cont)
	  | translateExp (IfExp (_, exp1, exp2, exp3), f, cont) =
	    let
		val cont' = Share (ref NONE, cont)
		val stms2 = translateExp (exp2, f, cont')
		val stms3 = translateExp (exp3, f, cont')
	    in
		simplifyIf (exp1, stms2, stms3)
	    end
	  | translateExp (WhileExp (info, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val cont' = Goto [O.IndirectStm (stmInfo info, r)]
		fun eval exp' = O.EvalStm (stmInfo (infoExp exp2), exp')
		val info' = infoExp exp1
		val id = freshId info'
		val trueBody = translateExp (exp2, eval, cont')
		val falseBody = translateExp (TupExp (info, nil), f, cont)
		val errorBody = [O.RaiseStm (stmInfo info', id_Match)]
		val stms1 =
		    [O.TestStm (stmInfo info', id,
				O.ConTest (id_true, NONE), trueBody,
				[O.TestStm (stmInfo info', id,
					    O.ConTest (id_false, NONE),
					    falseBody, errorBody)])]
		val stms2 =
		    translateDec (ValDec (info', VarPat (info', id), exp1),
				  Goto stms1)
		val stms = share stms2
	    in
		r := SOME stms; stms
	    end
	  | translateExp (SeqExp (_, exps), f, cont) =
	    let
		val isLast = ref true
		fun translate (exp, stms) =
		    if !isLast then
			(case stms of
			     nil => ()
			   | _ =>
			     raise Crash.Crash "ImperativePhase.translateExp";
			 isLast := false; translateExp (exp, f, cont))
		    else
			translateExp
			(exp, (fn exp' =>
			       O.EvalStm (stmInfo (infoExp exp), exp')),
			 Goto stms)
	    in
		List.foldr (fn (exp, stms) => translate (exp, stms)) nil exps
	    end
	  | translateExp (CaseExp (info, exp, matches), f, cont) =
	    let
		val cont' = Share (ref NONE, cont)
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (region (infoExp exp), pat,
			       translateExp (exp, f, cont')))
		    matches
	    in
		simplifyCase (info, exp, matches', id_Match, false)
	    end
	  | translateExp (RaiseExp (info, exp), _, _) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
	    in
		r := SOME [O.RaiseStm (stmInfo info, id)];
		stms
	    end
	  | translateExp (HandleExp (info, exp, matches), f, cont) =
	    let
		val info' = infoExp exp
		val id' = freshId info'
		val shared = ref 0
		val cont' = Goto [O.EndHandleStm (stmInfo info, shared)]
		fun f' exp' = O.ValDec (stmInfo info', id', exp', false)
		val tryBody = translateExp (exp, f', cont')
		val catchInfo = (region info, NONE)   (*--** provide type *)
		val catchId = freshId catchInfo
		val catchVarExp =
		    VarExp (catchInfo, ShortId (catchInfo, catchId))
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (region (infoExp exp), pat,
			       translateExp (exp, f', cont')))
		    matches
		val catchBody =
		    simplifyCase (info, catchVarExp, matches', catchId, true)
		val contBody =
		    translateExp  (VarExp (info', ShortId (info', id')),
				   f, cont)
	    in
		[O.HandleStm (stmInfo info, tryBody, catchId, catchBody,
			      contBody, shared)]
	    end
	  | translateExp (LetExp (_, decs, exp), f, cont) =
	    let
		val stms = translateExp (exp, f, cont)
	    in
		translateCont (Decs (decs, Goto stms))
	    end
	and simplifyIf (AndExp (_, exp1, exp2), thenStms, elseStms) =
	    let
		val elseStms' = share elseStms
		val thenStms' = simplifyIf (exp2, thenStms, elseStms')
	    in
		simplifyIf (exp1, thenStms', elseStms')
	    end
	  | simplifyIf (OrExp (_, exp1, exp2), thenStms, elseStms) =
	    let
		val thenStms' = share thenStms
		val elseStms' = simplifyIf (exp2, thenStms', elseStms)
	    in
		simplifyIf (exp1, thenStms', elseStms')
	    end
	  | simplifyIf (exp, thenStms, elseStms) =
	    let
		val info = infoExp exp
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
		val errStms = [O.RaiseStm (stmInfo info, id_Match)]
	    in
		r := SOME [O.TestStm (stmInfo info, id,
				      O.ConTest (id_true, NONE), thenStms,
				      [O.TestStm (stmInfo info, id,
						  O.ConTest (id_false, NONE),
						  elseStms, errStms)])];
		stms
	    end
	and checkReachability consequents =
	    List.app (fn (coord, ref bodyOpt) =>
		      if isSome bodyOpt then ()
		      else Error.warn (coord, "unreachable expression"))
	    consequents
	and simplifyCase (info, exp, matches, raiseId, isReraise) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
		val errStms =
		    if isReraise then [O.ReraiseStm (stmInfo info, raiseId)]
		    else [O.RaiseStm (stmInfo info, raiseId)]
		val (graph, consequents) = buildGraph (matches, errStms)
	    in
		r := SOME (translateGraph (graph, [(nil, id)]));
		checkReachability consequents;
		stms
	    end
	and translateFunBody (info, id, matches, return) =
	    let
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (region (infoExp exp), pat,
			       translateExp (exp, return, Goto nil))) matches
		fun errStmsFun () = [O.RaiseStm (stmInfo info, id_Match)]
		val argsBodyList =
		    List.map (fn (args, graph, mapping, consequents) =>
			      let
				  val stms = translateGraph (graph, mapping)
			      in
				  checkReachability consequents;
				  (args, stms)
			      end) (buildFunArgs (id, matches', errStmsFun))
	    in
		case argsBodyList of
		    (O.OneArg _, _)::_ => argsBodyList
		  | _ => (O.OneArg id, errStmsFun ())::argsBodyList
	    end
	and translateGraph (Node (pos, test, ref thenGraph, ref elseGraph,
				  status as ref (Optimized (_, _))), mapping) =
	    let
		val stms =
		    share (translateNode (pos, test, thenGraph, elseGraph,
					  mapping))
	    in
		status := Translated stms; stms
	    end
	  | translateGraph (Node (_, _, _, _, ref (Translated stms)), _) = stms
	  | translateGraph (Leaf (stms, stmsOptRef as ref NONE), _) =
	    let
		val stms' = share stms
	    in
		stmsOptRef := SOME stms'; stms'
	    end
	  | translateGraph (Leaf (_, ref (SOME stms)), _) = stms
	  | translateGraph (_, _) =
	    raise Crash.Crash "MatchCompilationPhase.translateGraph"
	and translateNode (pos, GuardTest (mapping0, exp),
			   thenGraph, elseGraph, mapping) =
	    let
		val info = infoExp exp
		val r = ref NONE
		val rest = [O.IndirectStm (stmInfo info, r)]
		val subst = mappingsToSubst (mapping0, mapping)
		val (stms, id) = unfoldTerm (substExp (exp, subst), Goto rest)
		val thenStms = translateGraph (thenGraph, mapping)
		val elseStms = translateGraph (elseGraph, mapping)
		val errStms = [O.RaiseStm (stmInfo info, id_Match)]
	    in
		r := SOME [O.TestStm (stmInfo info, id,
				      O.ConTest (id_true, NONE), thenStms,
				      [O.TestStm (stmInfo info, id,
						  O.ConTest (id_false, NONE),
						  elseStms, errStms)])];
		stms
	    end
	  | translateNode (pos, DecTest (mapping0, decs),
			   thenGraph, _, mapping) =
	    let
		val thenStms = translateGraph (thenGraph, mapping)
		val subst = mappingsToSubst (mapping0, mapping)
		val cont = Decs (List.map (fn dec => substDec (dec, subst))
				 decs, Goto thenStms)
	    in
		translateCont cont
	    end
	  | translateNode (pos, test, thenGraph, elseGraph, mapping) =
	    let
		val id = lookup (pos, mapping)
		val (stms, test', mapping') =
		    translateTest (test, pos, mapping)
	    in
		stms @ [O.TestStm (stmInfo (Source.nowhere, NONE), id, test',
				   translateGraph (thenGraph, mapping'),
				   translateGraph (elseGraph, mapping'))]
	    end
	and translateTest (LitTest lit, _, mapping) =
	    (nil, O.LitTest lit, mapping)
	  | translateTest (ConTest (longid, NONE), _, mapping) =
	    let
		val (stms, id) = translateLongid longid
	    in
		(stms, O.ConTest (id, NONE), mapping)
	    end
	  | translateTest (ConTest (longid, SOME typ), pos, mapping) =
	    let
		val (stms, id) = translateLongid longid
		val id' = freshId (Source.nowhere, SOME typ)
		val mapping' = (longidToLabel longid::pos, id')::mapping
	    in
		(stms, O.ConTest (id, SOME id'), mapping')
	    end
	  | translateTest (RefTest typ, pos, mapping) =
	    let
		val id = freshId (Source.nowhere, SOME typ)
		val mapping' = (Label.fromString "ref"::pos, id)::mapping
	    in
		(nil, O.RefTest id, mapping')
	    end
	  | translateTest (TupTest typs, pos, mapping) =
	    let
		val ids =
		    List.map (fn typ => freshId (Source.nowhere, SOME typ))
		    typs
		val mapping' =
		    Misc.List_foldli
		    (fn (i, id, mapping) =>
		     (Label.fromInt (i + 1)::pos, id)::mapping) mapping ids
	    in
		(nil, O.TupTest ids, mapping')
	    end
	  | translateTest (RecTest labelTypList, pos, mapping) =
	    let
		val labelIdList =
		    List.map (fn (label, typ) =>
			      (label, freshId (Source.nowhere, SOME typ)))
		    labelTypList
		val mapping' =
		    ListPair.foldr (fn ((label, _), (_, i), mapping) =>
				    (label::pos, i)::mapping)
		    mapping (labelTypList, labelIdList)
	    in
		(nil, O.RecTest labelIdList, mapping')
	    end
	  | translateTest (LabTest (label, typ), pos, mapping) =
	    let
		val id = freshId (Source.nowhere, SOME typ)
		val mapping' = ((label::pos), id)::mapping
	    in
		(nil, O.LabTest (label, id), mapping')
	    end
	  | translateTest (VecTest typs, pos, mapping) =
	    let
		val ids =
		    List.map (fn typ => freshId (Source.nowhere, SOME typ))
		    typs
		val mapping' =
		    Misc.List_foldli
		    (fn (i, id, mapping) =>
		     (Label.fromInt (i + 1)::pos, id)::mapping) mapping ids
	    in
		(nil, O.VecTest ids, mapping')
	    end
	  | translateTest ((GuardTest (_, _) | DecTest (_, _)), _, _) =
	    raise Crash.Crash "MatchCompilationPhase.translateTest"

	fun translate (imports, (exportExp, sign)) =
	    let
		fun export exp = O.ExportStm (stmInfo (infoExp exportExp), exp)
	    in
		(imports, (translateExp (exportExp, export, Goto nil), sign))
	    end
    end
