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

structure FlatteningPhase :> FLATTENING_PHASE =
    struct
	structure C = EmptyContext
	structure I = IntermediateGrammar
	structure O = FlatGrammar

	open I
	open IntermediateAux
	open SimplifyMatch

	val id_Match = Id ({region = Source.nowhere},
			   Prebound.valstamp_match,
			   Prebound.valname_match)
	val id_Bind = Id ({region = Source.nowhere},
			  Prebound.valstamp_bind,
			  Prebound.valname_bind)

	val label_true = Label.fromString "true"
	val label_false = Label.fromString "false"

	fun exp_true info =
	    TagExp (info, Lab ({region = #region info}, label_true), false)
	fun exp_false info =
	    TagExp (info, Lab ({region = #region info}, label_false), false)

	fun lookup (pos, (pos', id)::mappingRest) =
	    if pos = pos' then id
	    else lookup (pos, mappingRest)
	  | lookup (pos, nil) =
	    raise Crash.Crash "FlatteningPhase.lookup"

	fun mappingsToSubst (mapping0, mapping) =
	    List.map (fn (pos, id) => (id, lookup (pos, mapping))) mapping0

	fun stm_info region = {region = region, liveness = ref O.Unknown}

	fun share nil = nil
	  | share (stms as [O.SharedStm (_, _, _)]) = stms
	  | share (stms as stm::_) =
	    [O.SharedStm (stm_info (#region (O.infoStm stm)), stms,
			  Stamp.new ())]

	datatype continuation =
	    Decs of dec list * continuation
	  | Goto of O.body
	  | Share of O.body option ref * continuation

	(* Matching conArity up with args *)

	fun testConArity (args as O.OneArg _, O.Unary, app) = (app args, nil)
	  | testConArity (O.OneArg id, O.Tuple n, app) =
	    let
		val ids =
		    List.tabulate
		    (n, fn _ => freshId {region = Source.nowhere})
	    in
		(app (O.TupArgs ids),
		 [O.ValDec (stm_info Source.nowhere, id,
			    O.TupExp ({region = Source.nowhere}, ids))])
	    end
	  | testConArity (O.OneArg id, O.Record labels, app) =
	    let
		val labelIdList =
		    List.map (fn label =>
			      (label, freshId {region = Source.nowhere}))
		    labels
	    in
		(app (O.RecArgs labelIdList),
		 [O.ValDec (stm_info Source.nowhere, id,
			    O.RecExp ({region = Source.nowhere},
				      labelIdList))])
	    end
	  | testConArity (args as O.TupArgs _, O.Tuple _, app) =
	    (app args, nil)
	  | testConArity (args as O.RecArgs _, O.Record _, app) =
	    (app args, nil)
	  | testConArity (_, _, _) =
	    raise Crash.Crash "FlatteningPhase.testConArity"

	fun tagAppTest (label, n, args, conArity) =
	    testConArity (args, conArity,
			  fn args => O.TagAppTest (label, n, args))

	fun conAppTest (id, args, conArity) =
	    testConArity (args, conArity,
			  fn args => O.ConAppTest (id, args))

	fun expConArity (args as O.OneArg _, O.Unary, app) = (NONE, app args)
	  | expConArity (O.OneArg id, O.Tuple n, app) =
	    let
		val ids =
		    List.tabulate
		    (n, fn _ => freshId {region = Source.nowhere})
	    in
		(SOME (id, O.TupTest ids), app (O.TupArgs ids))
	    end
	  | expConArity (O.OneArg id, O.Record labels, app) =
	    let
		val labelIdList =
		    List.map (fn label =>
			      (label, freshId {region = Source.nowhere}))
		    labels
	    in
		(SOME (id, O.RecTest labelIdList), app (O.RecArgs labelIdList))
	    end
	  | expConArity (args as O.TupArgs _, O.Tuple _, app) =
	    (NONE, app args)
	  | expConArity (args as O.RecArgs _, O.Record _, app) =
	    (NONE, app args)
	  | expConArity (_, _, _) =
	    raise Crash.Crash "FlatteningPhase.expConArity"

	fun tagAppExp (info, label, n, args, conArity) =
	    expConArity (args, conArity,
			 fn args => O.TagAppExp (info, label, n, args))

	fun conAppExp (info, id, args, conArity) =
	    expConArity (args, conArity,
			 fn args => O.ConAppExp (info, id, args))

	(* Translation *)

	fun translateLongid (ShortId (_, id)) = (nil, id)
	  | translateLongid (LongId (info, longid, Lab (_, label))) =
	    let
		val (stms, id) = translateLongid longid
		val id' = Id (info, Stamp.new (), Name.InId)
		val n = ~1   (*--** compute *)
		val stm =
		    O.ValDec (stm_info (#region info), id',
			      O.SelAppExp (info, label, n, id))
	    in
		(stms @ [stm], id')
	    end

	fun decsToIdExpList (O.ValDec (_, id, exp')::rest, region) =
	    (id, exp')::decsToIdExpList (rest, region)
	  | decsToIdExpList (O.IndirectStm (_, ref bodyOpt)::rest, region) =
	    decsToIdExpList (valOf bodyOpt, region) @
	    decsToIdExpList (rest, region)
	  | decsToIdExpList (_::_, region) =
	    Error.error (region, "not admissible")
	  | decsToIdExpList (nil, _) = nil

	fun translateIf (info: exp_info, id, thenStms, elseStms, errStms) =
	    [O.TestStm (stm_info (#region info), id,
			[(O.TagTest (label_true, 1), thenStms)],
			[O.TestStm (stm_info (#region info), id,
				    [(O.TagTest (label_false, 0), elseStms)],
				    errStms)])]

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
		fun declare exp' = O.ValDec (stm_info (#region info), id, exp')
	    in
		translateExp (exp, declare, cont)
	    end
	  | translateDec (ValDec (info, pat, exp), cont) =
	    let
		val matches = [(#region info, pat, translateCont cont)]
	    in
		simplifyCase (#region info, exp, matches, id_Bind, false)
	    end
	  | translateDec (RecDec (info, decs), cont) =
	    let
		val (constraints, idExpList, aliases) = SimplifyRec.derec decs
		val aliasDecs =
		    List.map (fn (fromId, toId, info) =>
			      let
				  val toExp = O.VarExp (id_info info, toId)
			      in
				  O.ValDec (stm_info (#region (infoId fromId)),
					    fromId, toExp)
			      end) aliases
		val subst = List.map (fn (id1, id2, _) => (id1, id2)) aliases
		val decs' =
		    List.foldr
		    (fn ((id, exp), decs) =>
		     translateExp (substExp (exp, subst),
				   fn exp' =>
				   O.ValDec (stm_info (#region (infoExp exp)),
					     id, exp'),
				   Goto decs)) nil idExpList
		val idExpList' = decsToIdExpList (decs', #region info)
		val rest =
		    O.RecDec (stm_info (#region info), idExpList')::
		    aliasDecs @ translateCont cont
		val errStms =
		    share [O.RaiseStm (stm_info (#region info), id_Bind)]
	    in
		List.foldr
		(fn ((longid1, longid2), rest) =>
		 let
		     val (stms1, id1) = translateLongid longid1
		     val (stms2, id2) = translateLongid longid2
		 in
		     (*--** the following ConTest has wrong arity *)
		     stms1 @ stms2 @
		     [O.TestStm (stm_info (#region info), id1,
				 [(O.ConTest id2, rest)], errStms)]
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
		val id' = freshId (id_info info)
		fun declare exp' =
		    O.ValDec (stm_info (#region info), id', exp')
		val stms = translateExp (exp, declare, cont)
	    in
		(stms, id')
	    end
	and unfoldArgs (TupExp (_, exps), rest, true) =
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
	  | unfoldArgs (ProdExp (_, expFields), rest, true) =
	    let
		val (stms, labelIdList) =
		    List.foldr (fn (Field (_, Lab (_, label), exp),
				    (stms, labelIdList)) =>
				    let
					val (stms', id) =
					    unfoldTerm (exp, Goto stms)
				    in
					(stms', (label, id)::labelIdList)
				    end) (rest, nil) expFields
	    in
		case LabelSort.sort labelIdList of
		    (labelIdList', LabelSort.Tup _) =>
			(stms, O.TupArgs (List.map #2 labelIdList'))
		  | (labelIdList', LabelSort.Rec) =>
			(stms, O.RecArgs labelIdList')
	    end
	  | unfoldArgs (exp, rest, _) =
	    let
		val (stms, id) = unfoldTerm (exp, Goto rest)
	    in
		(stms, O.OneArg id)
	    end
	and translateExp (LitExp (info, lit), f, cont) =
	    f (O.LitExp (id_info info, lit))::translateCont cont
	  | translateExp (PrimExp (info, s), f, cont) =
	    f (O.PrimExp (id_info info, s))::translateCont cont
	  | translateExp (NewExp (info, isNAry), f, cont) =
	    f (O.NewExp (id_info info, makeConArity (#typ info, isNAry)))::
	    translateCont cont
	  | translateExp (VarExp (info, longid), f, cont) =
	    let
		val (stms, id) = translateLongid longid
	    in
		stms @ f (O.VarExp (id_info info, id))::translateCont cont
	    end
	  | translateExp (TagExp (info, Lab (_, label), isNAry), f, cont) =
	    f (O.TagExp (id_info info, label, labelToIndex (#typ info, label),
			 makeConArity (#typ info, isNAry)))::
	    translateCont cont
	  | translateExp (ConExp (info, longid, isNAry), f, cont) =
	    let
		val (stms, id) = translateLongid longid
	    in
		stms @ f (O.ConExp (id_info info, id,
				    makeConArity (#typ info, isNAry)))::
		translateCont cont
	    end
	  | translateExp (RefExp info, f, cont) =
	    f (O.RefExp (id_info info))::translateCont cont
	  | translateExp (TupExp (info, exps), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms, ids) =
		    List.foldr (fn (exp, (stms, ids)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', id::ids)
				end) (rest, nil) exps
	    in
		r := SOME (f (O.TupExp (id_info info, ids))::
			   translateCont cont);
		stms
	    end
	  | translateExp (ProdExp (info, expFields), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
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
		    case LabelSort.sort fields of
			(fields', LabelSort.Tup _) =>
			    O.TupExp (id_info info, List.map #2 fields')
		      | (fields', LabelSort.Rec) =>
			    O.RecExp (id_info info, fields')
	    in
		r := SOME (f exp'::translateCont cont);
		stms
	    end
	  | translateExp (SelExp (info, Lab (_, label)), f, cont) =
	    let
		val n = labelToIndex (#typ info, label)
	    in
		f (O.SelExp (id_info info, label, n))::translateCont cont
	    end
	  | translateExp (VecExp (info, exps), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms, ids) =
		    List.foldr (fn (exp, (stms, ids)) =>
				let
				    val (stms', id) =
					unfoldTerm (exp, Goto stms)
				in
				    (stms', id::ids)
				end) (rest, nil) exps
	    in
		r := SOME (f (O.VecExp (id_info info, ids))::
			   translateCont cont);
		stms
	    end
	  | translateExp (FunExp (info, matches), f, cont) =
	    let
		val matches' =
		    List.map (fn Match (info, pat, exp) =>
			      let
				  val region = #region info
				  fun return exp' =
				      O.ReturnStm (stm_info region, exp')
			      in
				  (#region (infoExp exp), pat,
				   translateExp (exp, return, Goto nil))
			      end)
		    matches
		val region = #region (infoMatch (List.hd matches))
		val errStms = [O.RaiseStm (stm_info region, id_Match)]
		val (args, graph, mapping, consequents) =
		    buildFunArgs (matches', errStms)
		val body = translateGraph (graph, mapping)
	    in
		checkReachability consequents;
		f (O.FunExp (id_info info, Stamp.new (), nil, args, body))::
		translateCont cont
	    end
(*--**DEBUG
	  | translateExp (FunExp (info, matches), f, cont) =
	    let
		val region = #region (infoMatch (List.hd matches))
		fun return exp' = O.ReturnStm (stm_info region, exp')
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (#region (infoExp exp), pat,
			       translateExp (exp, return, Goto nil))) matches
		val r = ref NONE
		val errStms = [O.IndirectStm (stm_info region, r)]
		val (args, graph, mapping, consequents) =
		    buildFunArgs (matches', errStms)
		val body = translateGraph (graph, mapping)
		val id = freshId info
	    in
		checkReachability consequents;
		r := SOME [O.ValDec (stm_info region, id,
				     O.ConAppExp (id_info info, id_Match,
						  args)),
			   O.RaiseStm (stm_info region, id)];
		f (O.FunExp (id_info info, Stamp.new (), nil, args, body))::
		translateCont cont
	    end
*)
	  | translateExp (AppExp (info, TagExp (info', Lab (_, label), isNAry),
				  exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms, args) = unfoldArgs (exp2, rest, isNAry)
		val n = labelToIndex (#typ info', label)
		val conArity = makeConArity (#typ info', isNAry)
		val (idTestOpt, exp') =
		    tagAppExp (id_info info, label, n, args, conArity)
	    in
		case idTestOpt of
		    NONE =>
			r := SOME (f exp'::translateCont cont)
		  | SOME (id, test) =>
			r := SOME ([O.TestStm (stm_info (#region info), id,
					       [(test,
						 f exp'::translateCont cont)],
					       nil)]);
		stms
	    end
	  | translateExp (AppExp (info, ConExp (info', longid, isNAry), exp2),
			  f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms2, args) = unfoldArgs (exp2, rest, isNAry)
		val (stms1, id1) = translateLongid longid
		val (idTestOpt, exp') =
		    conAppExp (id_info info, id1, args,
			       makeConArity (#typ info', isNAry))
	    in
		case idTestOpt of
		    NONE =>
			r := SOME (f exp'::translateCont cont)
		  | SOME (id, test) =>
			r := SOME ([O.TestStm (stm_info (#region info), id,
					       [(test,
						 f exp'::translateCont cont)],
					       nil)]);
		stms1 @ stms2
	    end
	  | translateExp (AppExp (info, RefExp _, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms2, id) = unfoldTerm (exp2, Goto rest)
	    in
		(r := SOME (f (O.RefAppExp (id_info info, id))::
			    translateCont cont);
		 stms2)
	    end
	  | translateExp (AppExp (info, SelExp (info', Lab (_, label)), exp2),
			  f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms2, id2) = unfoldTerm (exp2, Goto rest)
		val n = labelToIndex (#typ info', label)
	    in
		(r := SOME (f (O.SelAppExp (id_info info, label, n, id2))::
			    translateCont cont);
		 stms2)
	    end
	  | translateExp (AppExp (info, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms2, args) = unfoldArgs (exp2, rest, true)
		val (stms1, id1) = unfoldTerm (exp1, Goto stms2)
	    in
		r := SOME (f (O.VarAppExp (id_info info, id1, args))::
			   translateCont cont);
		stms1
	    end
	  | translateExp (AdjExp (info, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms2, id2) = unfoldTerm (exp2, Goto rest)
		val (stms1, id1) = unfoldTerm (exp1, Goto stms2)
	    in
		r := SOME (f (O.AdjExp (id_info info, id1, id2))::
			   translateCont cont);
		stms1
	    end
	  | translateExp (UpExp (_, exp), f, cont) =
	    translateExp (exp, f, cont)   (*--** UpExp *)
	  | translateExp (AndExp (info, exp1, exp2), f, cont) =
	    translateExp (IfExp (info, exp1, exp2, exp_false info), f, cont)
	  | translateExp (OrExp (info, exp1, exp2), f, cont) =
	    translateExp (IfExp (info, exp1, exp_true info, exp2), f, cont)
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
		val cont' = Goto [O.IndirectStm (stm_info (#region info), r)]
		fun eval exp' =
		    O.EvalStm (stm_info (#region (infoExp exp2)), exp')
		val info' = infoExp exp1
		val id = freshId (id_info info')
		val trueBody = translateExp (exp2, eval, cont')
		val falseBody = translateExp (TupExp (info, nil), f, cont)
		val errorBody =
		    [O.RaiseStm (stm_info (#region info'), id_Match)]
		val stms1 =
		    translateIf (info', id, trueBody, falseBody, errorBody)
		val stms2 =
		    translateDec (ValDec (id_info info',
					  VarPat (info', id), exp1),
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
			   | _::_ =>
			     raise Crash.Crash "FlatteningPhase.translateExp";
			 isLast := false; translateExp (exp, f, cont))
		    else
			translateExp
			(exp,
			 fn exp' =>
			 O.EvalStm (stm_info (#region (infoExp exp)), exp'),
			 Goto stms)
	    in
		List.foldr (fn (exp, stms) => translate (exp, stms)) nil exps
	    end
	  | translateExp (CaseExp (info, exp, matches), f, cont) =
	    let
		val cont' = Share (ref NONE, cont)
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (#region (infoExp exp), pat,
			       translateExp (exp, f, cont')))
		    matches
	    in
		simplifyCase (#region info, exp, matches', id_Match, false)
	    end
	  | translateExp (RaiseExp (info, exp), _, _) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
	    in
		r := SOME [O.RaiseStm (stm_info (#region info), id)];
		stms
	    end
	  | translateExp (HandleExp (info, exp, matches), f, cont) =
	    let
		val info' = infoExp exp
		val info'' = id_info info'
		val id' = freshId info''
		val stamp = Stamp.new ()
		val cont' =
		    Goto [O.EndHandleStm (stm_info (#region info), stamp)]
		fun f' exp' = O.ValDec (stm_info (#region info'), id', exp')
		val tryBody = translateExp (exp, f', cont')
		val catchInfo =
		    {region = #region info, typ = PreboundType.typ_exn}
		val catchId = freshId (id_info catchInfo)
		val catchVarExp =
		    VarExp (catchInfo, ShortId (id_info catchInfo, catchId))
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (#region (infoExp exp), pat,
			       translateExp (exp, f', cont')))
		    matches
		val catchBody =
		    simplifyCase (#region info, catchVarExp, matches',
				  catchId, true)
		val contBody =
		    translateExp (VarExp (info', ShortId (info'', id')),
				  f, cont)
	    in
		[O.HandleStm (stm_info (#region info), tryBody,
			      catchId, catchBody, contBody, stamp)]
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
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
		val errStms = [O.RaiseStm (stm_info (#region info), id_Match)]
		val stms1 = translateIf (info, id, thenStms, elseStms, errStms)
	    in
		r := SOME stms1;
		stms
	    end
	and checkReachability consequents =
	    List.app (fn (region, ref bodyOpt) =>
		      if isSome bodyOpt then ()
		      else Error.warn (region, "unreachable expression"))
	    consequents
	and simplifyCase (region, exp, matches, raiseId, isReraise) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info region, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
		val errStms =
		    if isReraise then
			[O.ReraiseStm (stm_info region, raiseId)]
		    else [O.RaiseStm (stm_info region, raiseId)]
		val (graph, consequents) = buildGraph (matches, errStms)
	    in
		r := SOME (translateGraph (graph, [(nil, id)]));
		checkReachability consequents;
		stms
	    end
(*--**DEBUG
	and simplifyCase (region, exp, matches, raiseId, isReraise) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info region, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)
		val id' = freshId (infoExp exp)
		val errStms =
		    [O.ValDec (stm_info region, id',
			       O.ConAppExp (infoExp exp, raiseId,
					    O.OneArg id)),
		     if isReraise then O.ReraiseStm (stm_info region, id')
		     else O.RaiseStm (stm_info region, id')]
		val (graph, consequents) = buildGraph (matches, errStms)
	    in
		r := SOME (translateGraph (graph, [(nil, id)]));
		checkReachability consequents;
		stms
	    end
*)
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
	    raise Crash.Crash "FlatteningPhase.translateGraph"
	and translateNode (pos, GuardTest (mapping0, exp),
			   thenGraph, elseGraph, mapping) =
	    let
		val info = infoExp exp
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val subst = mappingsToSubst (mapping0, mapping)
		val (stms, id) = unfoldTerm (substExp (exp, subst), Goto rest)
		val thenStms = translateGraph (thenGraph, mapping)
		val elseStms = translateGraph (elseGraph, mapping)
		val errStms = [O.RaiseStm (stm_info (#region info), id_Match)]
		val stms1 = translateIf (info, id, thenStms, elseStms, errStms)
	    in
		r := SOME stms1;
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
		val (stms, test', thenStms, mapping') =
		    translateTest (test, pos, mapping)
		val thenStms' = translateGraph (thenGraph, mapping')
	    in
		stms @ [O.TestStm (stm_info Source.nowhere, id,
				   [(test', thenStms @ thenStms')],
				   translateGraph (elseGraph, mapping'))]
	    end
	and translateTypArgs (O.OneArg typ, pos, mapping) =
	    let
		val id = freshId {region = Source.nowhere}
	    in
		(O.OneArg id, (pos, id)::mapping)
	    end
	  | translateTypArgs (O.TupArgs typs, pos, mapping) =
	    let
		val ids =
		    List.map (fn _ => freshId {region = Source.nowhere}) typs
	    in
		(O.TupArgs ids,
		 List.foldri
		 (fn (i, id, mapping) =>
		  (LABEL (Label.fromInt (i + 1))::pos, id)::mapping)
		 mapping ids)
	    end
	  | translateTypArgs (O.RecArgs labelTypList, pos, mapping) =
	    let
		val labelIdList =
		    List.map (fn (label, _) =>
			      (label, freshId {region = Source.nowhere}))
		    labelTypList
	    in
		(O.RecArgs labelIdList,
		 List.foldr (fn ((label, id), mapping) =>
			     (LABEL label::pos, id)::mapping)
		 mapping labelIdList)
	    end
	and translateTest (LitTest lit, _, mapping) =
	    (nil, O.LitTest lit, nil, mapping)
	  | translateTest (TagTest label, _, mapping) =
	    (nil, O.TagTest label, nil, mapping)
	  | translateTest (TagAppTest (label, n, args, conArity),
			   pos, mapping) =
	    let
		val (idArgs, mapping') =
		    translateTypArgs (args, LABEL label::pos, mapping)
		val (test, thenStms) = tagAppTest (label, n, idArgs, conArity)
	    in
		(nil, test, thenStms, mapping')
	    end
	  | translateTest (ConTest longid, _, mapping) =
	    let
		val (stms, id) = translateLongid longid
	    in
		(stms, O.ConTest id, nil, mapping)
	    end
	  | translateTest (ConAppTest (longid, args, conArity), pos, mapping) =
	    let
		val (stms, id) = translateLongid longid
		val (idArgs, mapping') =
		    translateTypArgs (args, longidToSelector longid::pos,
				      mapping)
		val (test, thenStms) = conAppTest (id, idArgs, conArity)
	    in
		(stms, test, thenStms, mapping')
	    end
	  | translateTest (RefAppTest typ, pos, mapping) =
	    let
		val id = freshId {region = Source.nowhere}
		val mapping' =
		    (LABEL (Label.fromString "ref")::pos, id)::mapping
	    in
		(nil, O.RefAppTest id, nil, mapping')
	    end
	  | translateTest (TupTest typs, pos, mapping) =
	    let
		val ids =
		    List.map (fn _ => freshId {region = Source.nowhere}) typs
		val mapping' =
		    List.foldli
		    (fn (i, id, mapping) =>
		     (LABEL (Label.fromInt (i + 1))::pos, id)::mapping)
		    mapping ids
	    in
		(nil, O.TupTest ids, nil, mapping')
	    end
	  | translateTest (RecTest labelTypList, pos, mapping) =
	    let
		val labelIdList =
		    List.map (fn (label, _) =>
			      (label, freshId {region = Source.nowhere}))
		    labelTypList
		val mapping' =
		    ListPair.foldr (fn ((label, _), (_, i), mapping) =>
				    (LABEL label::pos, i)::mapping)
		    mapping (labelTypList, labelIdList)
	    in
		(nil, O.RecTest labelIdList, nil, mapping')
	    end
	  | translateTest (LabTest (label, n, typ), pos, mapping) =
	    let
		val id = freshId {region = Source.nowhere}
		val mapping' = ((LABEL label::pos), id)::mapping
	    in
		(nil, O.LabTest (label, n, id), nil, mapping')
	    end
	  | translateTest (VecTest typs, pos, mapping) =
	    let
		val ids =
		    List.map (fn _ => freshId {region = Source.nowhere}) typs
		val mapping' =
		    List.foldli
		    (fn (i, id, mapping) =>
		     (LABEL (Label.fromInt (i + 1))::pos, id)::mapping)
		    mapping ids
	    in
		(nil, O.VecTest ids, nil, mapping')
	    end
	  | translateTest ((GuardTest (_, _) | DecTest (_, _)), _, _) =
	    raise Crash.Crash "FlatteningPhase.translateTest"

	fun translate () (desc, (imports, (exportExp, sign))) =
	    let
		fun export exp =
		    O.ExportStm (stm_info (#region (infoExp exportExp)), exp)
	    in
		(imports, (translateExp (exportExp, export, Goto nil), sign))
	    end
    end
