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

	val id_false = Id (Source.nowhere, Prebound.stamp_false, ExId "false")
	val id_true = Id (Source.nowhere, Prebound.stamp_true, ExId "true")
	val id_Match = Id (Source.nowhere, Prebound.stamp_Match, ExId "Match")
	val id_Bind = Id (Source.nowhere, Prebound.stamp_Bind, ExId "Bind")

	val longid_true = ShortId (Source.nowhere, id_true)
	val longid_false = ShortId (Source.nowhere, id_false)

	structure FieldLabelSort =
	    MakeLabelSort(type 'a t = string * id
			  fun get (s, _) = s)

	type mapping = (pos * id) list

	fun lookup (pos, (pos', id)::mappingRest) =
	    if pos = pos' then id
	    else lookup (pos, mappingRest)
	  | lookup (pos, nil) = Crash.crash "MatchCompilationPhase.lookup"

	fun mappingsToSubst (mapping0, mapping) =
	    List.map (fn (pos, id) => (id, lookup (pos, mapping))) mapping0

	(* Translation *)

	fun info coord = (coord, ref O.Unknown)

	fun share nil = nil
	  | share (stms as [O.SharedStm (_, _, _)]) = stms
	  | share stms = [O.SharedStm (info Source.nowhere, stms, ref 0)]

	datatype continuation =
	    Decs of dec list * continuation
	  | Goto of O.body
	  | Share of O.body option ref * continuation
	  | Export of O.exp

	fun translateLongid (ShortId (_, id)) = (nil, id)
	  | translateLongid (LongId (coord, longid, Lab (_, s))) =
	    let
		val (stms, id) = translateLongid longid
		val id' = freshId coord
		val stm =
		    O.ValDec (info coord, id',
			      O.SelAppExp (coord, s, id), false)
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
	  | translateCont (Export exp) =
	    [O.ExportStm (info Source.nowhere, exp)]
	and translateDec (ValDec (coord, VarPat (_, id), exp), cont) =
	    let
		fun declare exp' = O.ValDec (info coord, id, exp', false)
	    in
		translateExp (exp, declare, cont)
	    end
	  | translateDec (ValDec (coord, pat, exp), cont) =
	    let
		val matches = [(coord, pat, translateCont cont)]
	    in
		simplifyCase (coord, exp, matches, id_Bind)
	    end
	  | translateDec (RecDec (coord, decs), cont) =
	    let
		val (constraints, idExpList, subst) = SimplifyRec.derec decs
		val aliasDecs =
		    List.map (fn (fromId, toId) =>
			      let
				  val coord = infoId toId
				  val toExp = O.VarExp (coord, toId)
			      in
				  O.ValDec (info (infoId fromId),
					    fromId, toExp, false)
			      end) subst
		val decs' =
		    List.foldr (fn ((id, exp), decs) =>
				translateExp (substExp (exp, subst),
					      fn exp' =>
					      O.ValDec (info (infoExp exp),
							id, exp', false),
					      Goto decs)) nil idExpList
		val idExpList' = decsToIdExpList (decs', coord)
		val rest =
		    O.RecDec (info coord, idExpList', false)::aliasDecs @
		    translateCont cont
		val errStms = share [O.RaiseStm (info coord, id_Bind)]
	    in
		List.foldr
		(fn ((longid1, longid2, hasArgs), rest) =>
		 let
		     val (stms1, id1) = translateLongid longid1
		     val (stms2, id2) = translateLongid longid2
		 in
		     stms1 @ stms2 @
		     (if hasArgs then
			  let
			      val id1' = freshId coord
			      val id2' = freshId coord
			  in
			      [O.ValDec (info coord, id1',
					 O.ConAppExp (coord, id1,
						      O.OneArg id1), false),
			       O.TestStm (info coord, id1,
					  O.ConTest (id2, SOME id2'),
					  rest, errStms)]
			  end
		      else
			  [O.TestStm (info coord, id1, O.ConTest (id2, NONE),
				      rest, errStms)])
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
		val coord = infoExp exp
		val id' = freshId coord
		fun declare exp' = O.ValDec (info coord, id', exp', false)
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
	and translateExp (LitExp (coord, lit), f, cont) =
	    f (O.LitExp (coord, lit))::translateCont cont
	  | translateExp (PrimExp (coord, s), f, cont) =
	    f (O.PrimExp (coord, s))::translateCont cont
	  | translateExp (NewExp (coord, _, hasArgs), f, cont) =
	    f (O.NewExp (coord, hasArgs))::translateCont cont
	  | translateExp (VarExp (coord, longid), f, cont) =
	    let
		val (stms, id) = translateLongid longid
	    in
		stms @ f (O.VarExp (coord, id))::translateCont cont
	    end
	  | translateExp (ConExp (coord, longid, hasArgs), f, cont) =
	    let
		val (stms, id) = translateLongid longid
	    in
		stms @ f (O.ConExp (coord, id, hasArgs))::translateCont cont
	    end
	  | translateExp (RefExp coord, f, cont) =
	    f (O.RefExp coord)::translateCont cont
	  | translateExp (TupExp (coord, exps), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms, ids) =
		    List.foldr (fn (exp, (stms, ids)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', id::ids)
				end) (rest, nil) exps
	    in
		r := SOME (f (O.TupExp (coord, ids))::translateCont cont);
		stms
	    end
	  | translateExp (RowExp (coord, expFields), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms, fields) =
		    List.foldr (fn (Field (_, Lab (_, s), exp),
				    (stms, fields)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', (s, id)::fields)
				end) (rest, nil) expFields
		val exp' =
		    case FieldLabelSort.sort fields of
			(fields', FieldLabelSort.Tup _) =>
			    O.TupExp (coord, List.map #2 fields')
		      | (fields', FieldLabelSort.Rec) =>
			    O.RecExp (coord, fields')
	    in
		r := SOME (f exp'::translateCont cont);
		stms
	    end
	  | translateExp (SelExp (coord, Lab (_, s)), f, cont) =
	    f (O.SelExp (coord, s))::translateCont cont
	  | translateExp (VecExp (coord, exps), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms, ids) =
		    List.foldr (fn (exp, (stms, ids)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', id::ids)
				end) (rest, nil) exps
	    in
		r := SOME (f (O.VecExp (coord, ids))::translateCont cont);
		stms
	    end
	  | translateExp (FunExp (coord, id, exp), f, cont) =
	    let
		fun return exp' = O.ReturnStm (info (infoExp exp), exp')
		val argsBodyList =
		    case exp of
			CaseExp (_, VarExp (_, ShortId (_, id')), matches) =>
			    if idEq (id, id')
				andalso not (occursInMatches (matches, id))
			    then translateFunBody (coord, id, matches, return)
			    else
				[(O.OneArg id,
				  translateExp (exp, return, Goto nil))]
		      | _ =>
			    [(O.OneArg id,
			      translateExp (exp, return, Goto nil))]
	    in
		f (O.FunExp (coord, Stamp.new (), nil, argsBodyList))::
		translateCont cont
	    end
	  | translateExp (AppExp (coord, ConExp (_, longid, true), exp2),
			  f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms2, args) = unfoldArgs (exp2, rest)
		val (stms1, id1) = translateLongid longid
	    in
		r := SOME (f (O.ConAppExp (coord, id1, args))::
			   translateCont cont);
		stms1 @ stms2
	    end
	  | translateExp (AppExp (coord, RefExp _, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms2, args) = unfoldArgs (exp2, rest)
	    in
		(r := SOME (f (O.RefAppExp (coord, args))::translateCont cont);
		 stms2)
	    end
	  | translateExp (AppExp (coord, SelExp (_, Lab (_, s)), exp2),
			  f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms2, id2) = unfoldTerm (exp2, Goto rest)
	    in
		(r := SOME (f (O.SelAppExp (coord, s, id2))::
			    translateCont cont);
		 stms2)
	    end
	  | translateExp (AppExp (coord, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms2, args) = unfoldArgs (exp2, rest)
		val (stms1, id1) = unfoldTerm (exp1, Goto stms2)
	    in
		r := SOME (f (O.AppExp (coord, id1, args))::
			   translateCont cont);
		stms1
	    end
	  | translateExp (AdjExp (coord, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms2, id2) = unfoldTerm (exp2, Goto rest)
		val (stms1, id1) = unfoldTerm (exp1, Goto stms2)
	    in
		r := SOME (f (O.AdjExp (coord, id1, id2))::translateCont cont);
		stms1
	    end
	  | translateExp (AndExp (coord, exp1, exp2), f, cont) =
	    translateExp (IfExp (coord, exp1,
				 exp2, VarExp (coord, longid_false)), f, cont)
	  | translateExp (OrExp (coord, exp1, exp2), f, cont) =
	    translateExp (IfExp (coord, exp1,
				 VarExp (coord, longid_true), exp2), f, cont)
	  | translateExp (IfExp (_, exp1, exp2, exp3), f, cont) =
	    let
		val cont' = Share (ref NONE, cont)
		val stms2 = translateExp (exp2, f, cont')
		val stms3 = translateExp (exp3, f, cont')
	    in
		simplifyIf (exp1, stms2, stms3)
	    end
	  | translateExp (WhileExp (coord, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val cont' = Goto [O.IndirectStm (info coord, r)]
		fun eval exp' = O.EvalStm (info (infoExp exp2), exp')
		val coord' = infoExp exp1
		val id = freshId coord'
		val trueBody = translateExp (exp2, eval, cont')
		val falseBody = translateExp (TupExp (coord, nil), f, cont)
		val errorBody = [O.RaiseStm (info coord', id_Match)]
		val stms1 =
		    [O.TestStm (info coord', id,
				O.ConTest (id_true, NONE), trueBody,
				[O.TestStm (info coord', id,
					    O.ConTest (id_false, NONE),
					    falseBody, errorBody)])]
		val stms2 =
		    translateDec (ValDec (coord', VarPat (coord', id), exp1),
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
			   | _ => Crash.crash "ImperativePhase.translateExp";
			 isLast := false; translateExp (exp, f, cont))
		    else
			translateExp
			(exp, (fn exp' => O.EvalStm (info (infoExp exp), exp')),
			 Goto stms)
	    in
		List.foldr (fn (exp, stms) => translate (exp, stms)) nil exps
	    end
	  | translateExp (CaseExp (coord, exp, matches), f, cont) =
	    let
		val cont' = Share (ref NONE, cont)
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (infoExp exp, pat, translateExp (exp, f, cont')))
		    matches
	    in
		simplifyCase (coord, exp, matches', id_Match)
	    end
	  | translateExp (RaiseExp (coord, exp), _, _) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
	    in
		r := SOME [O.RaiseStm (info coord, id)];
		stms
	    end
	  | translateExp (HandleExp (coord, exp, matches), f, cont) =
	    let
		val coord' = infoExp exp
		val id' = freshId coord'
		val shared = ref 0
		val cont' = Goto [O.EndHandleStm (info coord, shared)]
		fun f' exp' = O.ValDec (info coord', id', exp', false)
		val tryBody = translateExp (exp, f', cont')
		val catchId = freshId coord
		val catchVarExp = VarExp (coord, ShortId (coord, catchId))
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (infoExp exp, pat, translateExp (exp, f', cont')))
		    matches
		val catchBody =
		    simplifyCase (coord, catchVarExp, matches', catchId)
		val contBody =
		    translateExp  (VarExp (coord', ShortId (coord', id')),
				   f, cont)
	    in
		[O.HandleStm (info coord, tryBody, catchId, catchBody,
			      contBody, shared)]
	    end
	  | translateExp (LetExp (coord, decs, exp), f, cont) =
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
		val coord = infoExp exp
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
		val errStms = [O.RaiseStm (info coord, id_Match)]
	    in
		r := SOME [O.TestStm (info coord, id,
				      O.ConTest (id_true, NONE), thenStms,
				      [O.TestStm (info coord, id,
						  O.ConTest (id_false, NONE),
						  elseStms, errStms)])];
		stms
	    end
	and checkReachability consequents =
	    List.app (fn (coord, ref bodyOpt) =>
		      if isSome bodyOpt then ()
		      else Error.warn (coord, "unreachable expression"))
	    consequents
	and simplifyCase (coord, exp, matches, raiseId) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
		val errStms = [O.RaiseStm (info coord, raiseId)]
		val (graph, consequents) = buildGraph (matches, errStms)
	    in
		r := SOME (translateGraph (graph, [(nil, id)]));
		checkReachability consequents;
		stms
	    end
	and translateFunBody (coord, id, matches, return) =
	    let
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (infoExp exp, pat,
			       translateExp (exp, return, Goto nil))) matches
		fun errStmsFun () = [O.RaiseStm (info coord, id_Match)]
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
	    Crash.crash "MatchCompilationPhase.translateGraph"
	and translateNode (pos, GuardTest (mapping0, exp),
			   thenGraph, elseGraph, mapping) =
	    let
		val coord = infoExp exp
		val r = ref NONE
		val rest = [O.IndirectStm (info coord, r)]
		val subst = mappingsToSubst (mapping0, mapping)
		val (stms, id) = unfoldTerm (substExp (exp, subst), Goto rest)
		val thenStms = translateGraph (thenGraph, mapping)
		val elseStms = translateGraph (elseGraph, mapping)
		val errStms = [O.RaiseStm (info coord, id_Match)]
	    in
		r := SOME [O.TestStm (info coord, id,
				      O.ConTest (id_true, NONE), thenStms,
				      [O.TestStm (info coord, id,
						  O.ConTest (id_false, NONE),
						  elseStms, errStms)])];
		stms
	    end
	  | translateNode (pos, DecTest (mapping0, coord, decs),
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
		stms @ [O.TestStm (info Source.nowhere, id, test',
				   translateGraph (thenGraph, mapping'),
				   translateGraph (elseGraph, mapping'))]
	    end
	and translateTest (LitTest lit, _, mapping) =
	    (nil, O.LitTest lit, mapping)
	  | translateTest (ConTest (longid, false), _, mapping) =
	    let
		val (stms, id) = translateLongid longid
	    in
		(stms, O.ConTest (id, NONE), mapping)
	    end
	  | translateTest (ConTest (longid, true), pos, mapping) =
	    let
		val (stms, id) = translateLongid longid
		val id' = freshId Source.nowhere
		val mapping' = ((""::pos), id')::mapping
	    in
		(stms, O.ConTest (id, SOME id'), mapping')
	    end
	  | translateTest (RefTest, pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((""::pos), id)::mapping
	    in
		(nil, O.RefTest id, mapping')
	    end
	  | translateTest (TupTest n, pos, mapping) =
	    let
		val ids = List.tabulate (n, fn _ => freshId Source.nowhere)
		val labs = List.tabulate (n, fn i => Int.toString (i + 1))
		val mapping' =
		    foldli (fn (i, id, mapping) =>
			    (Int.toString i::pos, id)::mapping) mapping ids
	    in
		(nil, O.TupTest ids, mapping')
	    end
	  | translateTest (RecTest labs, pos, mapping) =
	    let
		val stringIdList =
		    List.map (fn s => (s, freshId Source.nowhere)) labs
		val mapping' =
		    ListPair.foldr (fn (s, (_, i), mapping) =>
				    (s::pos, i)::mapping)
		    mapping (labs, stringIdList)
	    in
		(nil, O.RecTest stringIdList, mapping')
	    end
	  | translateTest (LabTest string, pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((string::pos), id)::mapping
	    in
		(nil, O.LabTest (string, id), mapping')
	    end
	  | translateTest (VecTest n, pos, mapping) =
	    let
		val ids = List.tabulate (n, fn _ => freshId Source.nowhere)
		val labs = List.tabulate (n, fn i => Int.toString (i + 1))
		val mapping' =
		    foldli (fn (i, id, mapping) =>
			    (Int.toString i::pos, id)::mapping) mapping ids
	    in
		(nil, O.VecTest ids, mapping')
	    end
	  | translateTest ((GuardTest (_, _) | DecTest (_, _, _)), _, _) =
	    Crash.crash "MatchCompilationPhase.translateTest"

	fun getPrintName (Id (_, _, ExId s)) = s
	  | getPrintName (Id (_, _, InId)) =
	    Crash.crash "MatchCompilationPhase.getPrintName"

	structure IdSort =
	    MakeLabelSort(type 'a t = id
			  val get = getPrintName)

	fun translate (imports, exports, decs) =
	    let
		val exportExp =
		    case IdSort.sort exports of
			(exports', IdSort.Tup _) =>
			    O.TupExp (Source.nowhere, exports')
		      | (exports', IdSort.Rec) =>
			    O.RecExp (Source.nowhere,
				      List.map (fn id => (getPrintName id, id))
				      exports')
	    in
		(imports, exports,
		 translateCont (Decs (decs, Export exportExp)))
	    end
    end
