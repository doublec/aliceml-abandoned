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

structure FlatteningPhase :> FLATTENING_PHASE =
    struct
	structure C = EmptyContext
	structure I = IntermediateGrammar
	structure O = FlatGrammar

	open I
	open IntermediateAux
	open SimplifyMatch

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

	fun testArity (O.OneArg id, O.Unary, app, body) =
	    app (O.OneArg (O.IdDef id), body)
	  | testArity (O.OneArg id, O.TupArity n, app, body) =
	    let
		val ids =
		    List.tabulate
		    (n, fn _ => freshId {region = Source.nowhere})
		val stm =
		    O.ValDec (stm_info Source.nowhere, O.IdDef id,
			      O.TupExp ({region = Source.nowhere}, ids))
	    in
		app (O.TupArgs (List.map O.IdDef ids), stm::body)
	    end
	  | testArity (O.OneArg id, O.ProdArity labels, app, body) =
	    let
		val labelIdList =
		    List.map (fn label =>
			      (label, freshId {region = Source.nowhere}))
		    labels
		val stm =
		    O.ValDec (stm_info Source.nowhere, O.IdDef id,
			      O.ProdExp ({region = Source.nowhere},
					 labelIdList))
		val labelIdDefList =
		    List.map (fn (label, id) => (label, O.IdDef id))
		    labelIdList
	    in
		app (O.ProdArgs labelIdDefList, stm::body)
	    end
	  | testArity (O.TupArgs ids, O.TupArity _, app, body) =
	    app (O.TupArgs (List.map O.IdDef ids), body)
	  | testArity (O.ProdArgs labelIdList, O.ProdArity _, app, body) =
	    app (O.ProdArgs (List.map (fn (label, id) =>
				       (label, O.IdDef id)) labelIdList), body)
	  | testArity (_, _, _, _) =
	    raise Crash.Crash "FlatteningPhase.testArity"

	fun tagAppTest (label, n, args, conArity, body) =
	    testArity (args, valOf conArity,
		       fn (args, body) =>
		       O.TagTests [(label, n, SOME args, body)], body)

	fun conAppTest (con, args, conArity, body) =
	    testArity (args, valOf conArity,
		       fn (args, body) =>
		       O.ConTests [(con, SOME args, body)], body)

	fun expArity (args as O.OneArg _, O.Unary, info, app) = (nil, app args)
	  | expArity (O.OneArg id, O.TupArity n, info: id_info, app) =
	    let
		val ids =
		    List.tabulate
		    (n, fn _ => freshId {region = Source.nowhere})
		val idDefs = List.map O.IdDef ids
	    in
		([O.TupDec (stm_info (#region info), idDefs, id)],
		 app (O.TupArgs ids))
	    end
	  | expArity (O.OneArg id, O.ProdArity labels, info, app) =
	    let
		val labelIdList =
		    List.map (fn label =>
			      (label, freshId {region = Source.nowhere}))
		    labels
		val labelIdDefList =
		    List.map (fn (label, id) => (label, O.IdDef id))
		    labelIdList
	    in
		([O.ProdDec (stm_info (#region info), labelIdDefList, id)],
		 app (O.ProdArgs labelIdList))
	    end
	  | expArity (args as O.TupArgs _, O.TupArity _, _, app) =
	    (nil, app args)
	  | expArity (args as O.ProdArgs _, O.ProdArity _, _, app) =
	    (nil, app args)
	  | expArity (_, _, _, _) =
	    raise Crash.Crash "FlatteningPhase.expArity"

	fun tagAppExp (info, label, n, args, conArity) =
	    expArity (args, valOf conArity, info,
		      fn args => O.TagAppExp (info, label, n, args))

	fun conAppExp (info, id, args, conArity) =
	    expArity (args, valOf conArity, info,
		      fn args => O.ConAppExp (info, id, args))

	(* Translation *)

	fun translateLongid (ShortId (info, id)) = (nil, id, #typ info)
	  | translateLongid (LongId ({region, typ = typOpt}, longid,
				     Lab (_, label))) =
	    let
		val (stms, id, typOpt') = translateLongid longid
		val info = {region = region}
		val id' = Id (info, Stamp.new (), Name.InId)
		val n = selIndex (valOf typOpt', label)
		val stm =
		    O.ValDec (stm_info region, O.IdDef id',
			      O.SelAppExp (info, label, n, id))
	    in
		(stms @ [stm], id', typOpt)
	    end

	fun decsToIdDefExpList (O.ValDec (_, idDef, exp')::rest, region) =
	    (idDef, exp')::decsToIdDefExpList (rest, region)
	  | decsToIdDefExpList (O.IndirectStm (_, ref bodyOpt)::rest, region) =
	    decsToIdDefExpList (valOf bodyOpt, region) @
	    decsToIdDefExpList (rest, region)
	  | decsToIdDefExpList (_::_, region) =
	    Error.error (region, "not admissible")
	  | decsToIdDefExpList (nil, _) = nil

	fun translateIf (info: exp_info, id, thenStms, elseStms, errStms) =
	    [O.TestStm (stm_info (#region info), id,
			O.TagTests [(PervasiveType.lab_true, 1, NONE,
				     thenStms),
				    (PervasiveType.lab_false, 0, NONE,
				     elseStms)], errStms)]

	fun raisePrim (region, name) =
	    let
		val info = {region = region}
		val id = freshId info
	    in
		[O.ValDec (stm_info region, O.IdDef id,
			   O.PrimExp (info, name)),
		 O.RaiseStm (stm_info region, id)]
	    end

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
		fun declare exp' =
		    O.ValDec (stm_info (#region info), O.IdDef id, exp')
	    in
		translateExp (exp, declare, cont)
	    end
	  | translateDec (ValDec (info, pat, exp), cont) =
	    let
		val matches = [(#region info, pat, translateCont cont)]
		val info = {region = #region info, typ = PervasiveType.typ_exn}
	    in
		simplifyCase (#region info, exp, matches,
			      PrimExp (info, "General.Bind"), false)
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
					    O.IdDef fromId, toExp)
			      end) aliases
		val subst = List.map (fn (id1, id2, _) => (id1, id2)) aliases
		val decs' =
		    List.foldr
		    (fn ((id, exp), decs) =>
		     translateExp (substExp (exp, subst),
				   fn exp' =>
				   O.ValDec (stm_info (#region (infoExp exp)),
					     O.IdDef id, exp'),
				   Goto decs)) nil idExpList
		val idDefExpList' = decsToIdDefExpList (decs', #region info)
		val rest =
		    O.RecDec (stm_info (#region info), idDefExpList')::
		    aliasDecs @ translateCont cont
		val errStms = share (raisePrim (#region info, "General.Bind"))
	    in
		List.foldr
		(fn ((longid1, longid2), rest) =>
		 let
		     val (stms1, id1, _) = translateLongid longid1
		     val (stms2, id2, _) = translateLongid longid2
		 in
		     (* the following ConTest has `wrong' arity *)
		     stms1 @ stms2 @
		     [O.TestStm (stm_info (#region info), id1,
				 O.ConTests [(O.Con id2, NONE, rest)],
				 errStms)]
		 end) rest constraints
	    end
	and unfoldTerm (VarExp (_, longid), cont) =
	    let
		val (stms, id, _) = translateLongid longid
	    in
		(stms @ translateCont cont, id)
	    end
	  | unfoldTerm (exp, cont) =
	    let
		val info = infoExp exp
		val id' = freshId (id_info info)
		fun declare exp' =
		    O.ValDec (stm_info (#region info), O.IdDef id', exp')
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
		  | (labelIdList', LabelSort.Prod) =>
			(stms, O.ProdArgs labelIdList')
	    end
	  | unfoldArgs (exp, rest, _) =
	    let
		val (stms, id) = unfoldTerm (exp, Goto rest)
	    in
		(stms, O.OneArg id)
	    end
	and translateExp (LitExp (info, lit), f, cont) =
	    f (O.LitExp (id_info info, lit))::translateCont cont
	  | translateExp (PrimExp (info, name), f, cont) =
	    f (O.PrimExp (id_info info, name))::translateCont cont
	  | translateExp (NewExp (info, isNAry), f, cont) =
	    f (O.NewExp (id_info info, makeConArity (#typ info, isNAry)))::
	    translateCont cont
	  | translateExp (VarExp (info, longid), f, cont) =
	    let
		val (stms, id, _) = translateLongid longid
	    in
		stms @ f (O.VarExp (id_info info, id))::translateCont cont
	    end
	  | translateExp (TagExp (info, Lab (_, label), isNAry), f, cont) =
	    f (O.TagExp (id_info info, label, tagIndex (#typ info, label),
			 makeConArity (#typ info, isNAry)))::
	    translateCont cont
	  | translateExp (ConExp (info, longid, isNAry), f, cont) =
	    let
		val (stms, id, _) = translateLongid longid
	    in
		stms @ f (O.ConExp (id_info info, O.Con id,
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
		      | (fields', LabelSort.Prod) =>
			    O.ProdExp (id_info info, fields')
	    in
		r := SOME (f exp'::translateCont cont);
		stms
	    end
	  | translateExp (SelExp (info, Lab (_, label)), f, cont) =
	    let
		val n = selIndex (#1 (Type.asArrow (#typ info)), label)
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
			      end) matches
		val region = #region (infoMatch (List.hd matches))
		val errStms = raisePrim (region, "General.Match")
		val (args, graph, mapping, consequents) =
		    buildFunArgs (matches', errStms)
		val body = translateGraph (graph, mapping)
	    in
		checkReachability consequents;
		f (O.FunExp (id_info info, Stamp.new (), nil, args, body))::
		translateCont cont
	    end
	  | translateExp (AppExp (info, TagExp (info', Lab (_, label), isNAry),
				  exp2), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms, args) = unfoldArgs (exp2, rest, isNAry)
		val n = tagIndex (#typ info', label)
		val conArity = makeConArity (#typ info', isNAry)
		val (stms', exp') =
		    tagAppExp (id_info info, label, n, args, conArity)
	    in
		r := SOME (stms' @ f exp'::translateCont cont);
		stms
	    end
	  | translateExp (AppExp (info, ConExp (info', longid, isNAry), exp2),
			  f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val (stms2, args) = unfoldArgs (exp2, rest, isNAry)
		val (stms1, id1, _) = translateLongid longid
		val conArity = makeConArity (#typ info', isNAry)
		val (stms', exp') =
		    conAppExp (id_info info, O.Con id1, args, conArity)
	    in
		r := SOME (stms' @ f exp'::translateCont cont);
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
		val n = selIndex (#1 (Type.asArrow (#typ info')), label)
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
		val arity1 = typToArity (#typ (infoExp exp1))
		val arity2 = typToArity (#typ (infoExp exp2))
		val info' = id_info info
		val region = #region info'
		fun sel label =
		    let
			val id = freshId info'
			val exp =
			    case findLabel (arity2, label) of
				SOME i =>
				    O.SelAppExp (info', label, i, id2)
			      | NONE =>
				    O.SelAppExp (info', label,
						 valOf (findLabel
							(arity1, label)), id1)
		    in
			(O.ValDec (stm_info region, O.IdDef id, exp), id)
		    end
		val (stms3, exp') =
		    case typToArity (#typ info) of
			O.Unary =>
			    raise Crash.Crash
				"FlatteningPhase.translateExp: AdjExp"
		      | O.TupArity n =>
			    let
				fun selInt i = sel (Label.fromInt (i + 1))
				val (stms, ids) =
				    ListPair.unzip (List.tabulate (n, selInt))
			    in
				(stms, O.TupExp (info', ids))
			    end
		      | O.ProdArity labels =>
			    let
				val (stms, labelIdList) =
				    ListPair.unzip
				    (List.map (fn label =>
					       let
						   val (stm, id) = sel label
					       in
						   (stm, (label, id))
					       end) labels)
			    in
				(stms, O.ProdExp (info', labelIdList))
			    end
	    in
		r := SOME (stms3 @ f exp'::translateCont cont); stms1
	    end
	  | translateExp (AndExp (info, exp1, exp2), f, cont) =
	    let
		val exp3 =
		    TagExp (info, Lab (id_info info, PervasiveType.lab_false),
			    false)
	    in
		translateExp (IfExp (info, exp1, exp2, exp3), f, cont)
	    end
	  | translateExp (OrExp (info, exp1, exp2), f, cont) =
	    let
		val exp3 =
		    TagExp (info, Lab (id_info info, PervasiveType.lab_true),
			    false)
	    in
		translateExp (IfExp (info, exp1, exp3, exp2), f, cont)
	    end
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
		    O.ValDec (stm_info (#region (infoExp exp2)),
			      O.Wildcard, exp')
		val info' = infoExp exp1
		val id = freshId (id_info info')
		val trueBody = translateExp (exp2, eval, cont')
		val falseBody = translateExp (TupExp (info, nil), f, cont)
		val errorBody = raisePrim (#region info', "General.Match")
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
			 fn exp' => O.ValDec (stm_info (#region (infoExp exp)),
					      O.Wildcard, exp'),
			 Goto stms)
	    in
		List.foldr translate nil exps
	    end
	  | translateExp (CaseExp (info, exp, matches), f, cont) =
	    let
		val cont' = Share (ref NONE, cont)
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (#region (infoExp exp), pat,
			       translateExp (exp, f, cont'))) matches
		val info = {region = #region info, typ = PervasiveType.typ_exn}
	    in
		simplifyCase (#region info, exp, matches',
			      PrimExp (info, "General.Match"), false)
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
		val id' = freshId (id_info info')
		val stamp = Stamp.new ()
		val cont' =
		    Goto [O.EndHandleStm (stm_info (#region info), stamp)]
		fun f' exp' =
		    O.ValDec (stm_info (#region info'), O.IdDef id', exp')
		val tryBody = translateExp (exp, f', cont')
		val catchInfo = {region = #region info,
				 typ = PervasiveType.typ_exn}
		val catchId = freshId (id_info catchInfo)
		val catchVarExp =
		    VarExp (catchInfo,
			    ShortId (longid_info catchInfo, catchId))
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (#region (infoExp exp), pat,
			       translateExp (exp, f', cont')))
		    matches
		val catchBody =
		    simplifyCase (#region info, catchVarExp, matches',
				  catchVarExp, true)
		val contBody =
		    translateExp (VarExp (info',
					  ShortId (longid_info info', id')),
				  f, cont)
	    in
		[O.HandleStm (stm_info (#region info), tryBody,
			      O.IdDef catchId, catchBody, contBody, stamp)]
	    end
	  | translateExp (FailExp info, f, cont) =
	    let
		val region = #region info
		val info' = {region = region}
		val holeId = freshId info'
		val exnId = freshId info'
	    in
		O.ValDec (stm_info region, O.IdDef holeId,
			  O.PrimAppExp (info', "Hole.hole", nil))::
		O.ValDec (stm_info region, O.IdDef exnId,
			  O.PrimExp (info', "Hole.Hole"))::
		O.ValDec (stm_info region, O.Wildcard,
			  O.PrimAppExp (info', "Hole.fail", [holeId, exnId]))::
		f (O.VarExp (info', holeId))::translateCont cont
	    end
	  | translateExp (LazyExp (info as {region, typ}, exp), f, cont) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info region, r)]
		val funInfo = {region = region,
			       typ = Type.inArrow (Type.inTuple #[], typ)}
		val pat = JokPat {region = region, typ = Type.inTuple #[]}
		val funExp = FunExp (funInfo, [Match (id_info info, pat, exp)])
		val (stms, id) = unfoldTerm (funExp, Goto rest)
	    in
		(r := SOME (f (O.PrimAppExp (id_info info, "Future.byneed",
					     [id]))::translateCont cont);
		 stms)
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
		val errStms = raisePrim (#region info, "General.Match")
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
	and simplifyCase (region, exp, matches, raiseExp, isReraise) =
	    let
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info region, r)]
		val (stms, id) = unfoldTerm (exp, Goto rest)

		val r' = ref NONE
		val rest' = [O.IndirectStm (stm_info region, r')]
		val (errStms, raiseId) = unfoldTerm (raiseExp, Goto rest')
		val (graph, consequents) = buildGraph (matches, errStms)
	    in
		r := SOME (translateGraph (graph, [(nil, id)]));
		r' := SOME (if isReraise then
				[O.ReraiseStm (stm_info region, raiseId)]
			    else [O.RaiseStm (stm_info region, raiseId)]);
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
		    [O.ValDec (stm_info region, O.IdDef id',
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
				  status as ref (Cooked (_, _))), mapping) =
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
	and translateNode (pos, RefAppTest _, thenGraph, _, mapping) =
	    let
		val id = freshId {region = Source.nowhere}
		val mapping' =
		    (LABEL (Label.fromString "ref")::pos, id)::mapping
		val id' = lookup (pos, mapping)
	    in
		O.RefAppDec (stm_info Source.nowhere, O.IdDef id, id')::
		translateGraph (thenGraph, mapping')
	    end
	  | translateNode (pos, TupTest typs, thenGraph, _, mapping) =
	    let
		val ids =
		    List.map (fn _ => freshId {region = Source.nowhere}) typs
		val mapping' =
		    List.foldli
		    (fn (i, id, mapping) =>
		     (LABEL (Label.fromInt (i + 1))::pos, id)::mapping)
		    mapping ids
		val id = lookup (pos, mapping)
	    in
		O.TupDec (stm_info Source.nowhere, List.map O.IdDef ids, id)::
		translateGraph (thenGraph, mapping')
	    end
	  | translateNode (pos, ProdTest labelTypList, thenGraph, _, mapping) =
	    let
		val labelIdList =
		    List.map (fn (label, _) =>
			      (label, freshId {region = Source.nowhere}))
		    labelTypList
		val mapping' =
		    ListPair.foldr (fn ((label, _), (_, id), mapping) =>
				    (LABEL label::pos, id)::mapping)
		    mapping (labelTypList, labelIdList)
		val labelIdDefList =
		    List.map (fn (label, id) => (label, O.IdDef id))
		    labelIdList
		val id = lookup (pos, mapping)
	    in
		O.ProdDec (stm_info Source.nowhere, labelIdDefList, id)::
		translateGraph (thenGraph, mapping')
	    end
	  | translateNode (pos, LabTest (label, n, _), thenGraph, _, mapping) =
	    let
		val id = freshId {region = Source.nowhere}
		val mapping' = ((LABEL label::pos), id)::mapping
	    in
		O.ValDec (stm_info Source.nowhere, O.IdDef id,
			  O.SelAppExp ({region = Source.nowhere}, label, n,
				       lookup (pos, mapping)))::
		translateGraph (thenGraph, mapping')
	    end
	  | translateNode (_, GuardTest (mapping0, exp),
			   thenGraph, elseGraph, mapping) =
	    let
		val info = infoExp exp
		val r = ref NONE
		val rest = [O.IndirectStm (stm_info (#region info), r)]
		val subst = mappingsToSubst (mapping0, mapping)
		val (stms, id) = unfoldTerm (substExp (exp, subst), Goto rest)
		val thenStms = translateGraph (thenGraph, mapping)
		val elseStms = translateGraph (elseGraph, mapping)
		val errStms = raisePrim (#region info, "General.Match")
		val stms1 = translateIf (info, id, thenStms, elseStms, errStms)
	    in
		r := SOME stms1;
		stms
	    end
	  | translateNode (_, DecTest (mapping0, decs),
			   thenGraph, _, mapping) =
	    let
		val thenStms = translateGraph (thenGraph, mapping)
		val subst = mappingsToSubst (mapping0, mapping)
		val cont = Decs (List.map (fn dec => substDec (dec, subst))
				 decs, Goto thenStms)
	    in
		translateCont cont
	    end
	  | translateNode (pos, LitTest lit, thenGraph, elseGraph, mapping) =
	    let
		val id = lookup (pos, mapping)
		val body = translateGraph (thenGraph, mapping)
		val tests = O.LitTests [(lit, body)]
	    in
		[O.TestStm (stm_info Source.nowhere, id, tests,
			    translateGraph (elseGraph, mapping))]
	    end
	  | translateNode (pos, TagTest (label, n),
			   thenGraph, elseGraph, mapping) =
	    let
		val id = lookup (pos, mapping)
		val body = translateGraph (thenGraph, mapping)
		val tests = O.TagTests [(label, n, NONE, body)]
	    in
		[O.TestStm (stm_info Source.nowhere, id, tests,
			    translateGraph (elseGraph, mapping))]
	    end
	  | translateNode (pos, TagAppTest (label, n, args, conArity),
			   thenGraph, elseGraph, mapping) =
	    let
		val id = lookup (pos, mapping)
		val (idArgs, mapping') =
		    translateTypArgs (args, LABEL label::pos, mapping)
		val body = translateGraph (thenGraph, mapping')
		val tests = tagAppTest (label, n, idArgs, conArity, body)
	    in
		[O.TestStm (stm_info Source.nowhere, id, tests,
			    translateGraph (elseGraph, mapping))]
	    end
	  | translateNode (pos, ConTest longid,
			   thenGraph, elseGraph, mapping) =
	    let
		val id = lookup (pos, mapping)
		val (stms, id', _) = translateLongid longid
		val body = translateGraph (thenGraph, mapping)
		val tests = O.ConTests [(O.Con id', NONE, body)]
	    in
		stms @ [O.TestStm (stm_info Source.nowhere, id, tests,
				   translateGraph (elseGraph, mapping))]
	    end
	  | translateNode (pos, ConAppTest (longid, args, conArity),
			   thenGraph, elseGraph, mapping) =
	    let
		val id = lookup (pos, mapping)
		val (stms, id', _) = translateLongid longid
		val (idArgs, mapping') =
		    translateTypArgs (args, longidToSelector longid::pos,
				      mapping)
		val body = translateGraph (thenGraph, mapping')
		val tests = conAppTest (O.Con id', idArgs, conArity, body)
	    in
		stms @ [O.TestStm (stm_info Source.nowhere, id, tests,
				   translateGraph (elseGraph, mapping))]
	    end
	  | translateNode (pos, VecTest typs, thenGraph, elseGraph, mapping) =
	    let
		val id = lookup (pos, mapping)
		val ids =
		    List.map (fn _ => freshId {region = Source.nowhere}) typs
		val mapping' =
		    List.foldli
		    (fn (i, id, mapping) =>
		     (LABEL (Label.fromInt (i + 1))::pos, id)::mapping)
		    mapping ids
		val body = translateGraph (thenGraph, mapping')
		val tests = O.VecTests [(List.map O.IdDef ids, body)]
	    in
		[O.TestStm (stm_info Source.nowhere, id, tests,
			    translateGraph (elseGraph, mapping))]
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
	  | translateTypArgs (O.ProdArgs labelTypList, pos, mapping) =
	    let
		val labelIdList =
		    List.map (fn (label, _) =>
			      (label, freshId {region = Source.nowhere}))
		    labelTypList
	    in
		(O.ProdArgs labelIdList,
		 List.foldr (fn ((label, id), mapping) =>
			     (LABEL label::pos, id)::mapping)
		 mapping labelIdList)
	    end

	fun translate () (desc, (imports, (exportExp, sign))) =
	    let
		fun export exp =
		    O.ExportStm (stm_info (#region (infoExp exportExp)), exp)
		val imports' =
		    List.map (fn (id, sign, url) => (O.IdDef id, sign, url))
		    imports
	    in
		(imports', (translateExp (exportExp, export, Goto nil), sign))
	    end
    end
