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

structure SimplifyRec :> SIMPLIFY_REC =
    struct
	structure I = IntermediateGrammar

	open I
	val region = IntermediateInfo.region

	type constraint = longid * longid * bool   (* has args *)
	type binding = id * exp
	type alias = id * id

	datatype pat =
	    WildPat of info
	  | LitPat of info * lit
	  | VarPat of info * id
	  | ConPat of info * longid * pat option
	  | RefPat of info * pat
	  | TupPat of info * pat list
	  | RowPat of info * pat field list * bool
	  | VecPat of info * pat list
	  | AsPat of info * id * pat

	fun infoPat (WildPat info) = info
	  | infoPat (LitPat (info, _)) = info
	  | infoPat (VarPat (info, _)) = info
	  | infoPat (ConPat (info, _, _)) = info
	  | infoPat (RefPat (info, _)) = info
	  | infoPat (TupPat (info, _)) = info
	  | infoPat (RowPat (info, _, _)) = info
	  | infoPat (VecPat (info, _)) = info
	  | infoPat (AsPat (info, _, _)) = info

	structure FieldSort =
	    MakeLabelSort(type 'a t = 'a field
			  fun get (Field (_, Lab (_, s), _)) = s)

	fun select (Field (_, Lab (_, s), x)::fieldr, s') =
	    if s = s' then SOME x else select (fieldr, s')
	  | select (nil, _) = NONE

	fun isSubarity (xs as Field (_, Lab (_, s), _)::xr,
			Field (_, Lab (_, s'), _)::yr) =
	    if s = s' then isSubarity (xr, yr)
	    else isSubarity (xs, yr)
	  | isSubarity (nil, _) = true
	  | isSubarity (_, nil) = false

	fun unalias (WildPat _) = (nil, NONE)
	  | unalias (VarPat (_, id)) = ([id], NONE)
	  | unalias (AsPat (_, id, pat)) =
	    let
		val (ids, patOpt) = unalias pat
	    in
		(id::ids, patOpt)
	    end
	  | unalias pat = (nil, SOME pat)

	fun patToExp (WildPat info) =
	    let
		val id = IntermediateAux.freshId info
	    in
		(VarPat (info, id), VarExp (info, ShortId (info, id)))
	    end
	  | patToExp (pat as LitPat (info, lit)) = (pat, LitExp (info, lit))
	  | patToExp (pat as VarPat (info, id)) =
	    (pat, VarExp (info, ShortId (info, id)))
	  | patToExp (pat as ConPat (info, longid, NONE)) =
	    (pat, ConExp (info, longid, false))
	  | patToExp (ConPat (info, longid, SOME pat)) =
	    let
		val (pat', exp') = patToExp pat
	    in
		(ConPat (info, longid, SOME pat'),
		 AppExp (info, ConExp (infoPat pat, longid, true), exp'))
	    end
	  | patToExp (RefPat (info, pat)) =
	    let
		val (pat', exp') = patToExp pat
		val info' = (#1 info, NONE)   (*--** *)
	    in
		(RefPat (info, pat'), AppExp (info, RefExp info', exp'))
	    end
	  | patToExp (TupPat (info, pats)) =
	    let
		val (pats', exps') = ListPair.unzip (List.map patToExp pats)
	    in
		(TupPat (info, pats'), TupExp (info, exps'))
	    end
	  | patToExp (RowPat (info, patFields, hasDots)) =
	    (*--** record patterns with dots must be resolved using the rhs *)
	    raise Crash.Crash "SimplifyRec.patToExp"
	  | patToExp (VecPat (info, pats)) =
	    let
		val (pats', exps') = ListPair.unzip (List.map patToExp pats)
	    in
		(VecPat (info, pats'), VecExp (info, exps'))
	    end
	  | patToExp (pat as AsPat (info, id, _)) =
	    (pat, VarExp (info, ShortId (info, id)))

	fun derec' (WildPat _, exp) = (nil, [(nil, exp)])
	  | derec' (LitPat (info, lit1), LitExp (_, lit2)) =
	    if lit1 = lit2 then (nil, nil)
	    else Error.error (region info, "pattern never matches")
	  | derec' (VarPat (_, id), exp) = (nil, [([id], exp)])
	  | derec' (ConPat (_, longid1, NONE),
		    ConExp (_, longid2, false)) =
	    ([(longid1, longid2, false)], nil)
	  | derec' (ConPat (_, longid1, SOME pat),
		   AppExp (_, ConExp (_, longid2, true), exp)) =
	    let
		val (constraints, idsExpList) = derec' (pat, exp)
	    in
		((longid1, longid2, true)::constraints, idsExpList)
	    end
	  | derec' (RefPat (_, pat), AppExp (_, RefExp _, exp)) =
	    derec' (pat, exp)
	  | derec' (TupPat (info, pats), TupExp (_, exps)) =
	    if length pats = length exps then
		ListPair.foldr (fn (pat, exp, (cr, idsExpr)) =>
				let
				    val (cs, idsExps) = derec' (pat, exp)
				in
				    (cs @ cr, idsExps @ idsExpr)
				end) (nil, nil) (pats, exps)
	    else Error.error (region info, "pattern never matches")
	  | derec' (TupPat (info, pats), RowExp (_, expFields)) =
	    (case FieldSort.sort expFields of
		 (expFields', FieldSort.Tup n) =>
		     if length pats = n then
			 ListPair.foldr
			 (fn (pat, Field (_, _, exp), (cr, idsExpr)) =>
			  let
			      val (cs, idsExps) = derec' (pat, exp)
			  in
			      (cs @ cr, idsExps @ idsExpr)
			  end) (nil, nil) (pats, expFields')
		     else Error.error (region info, "pattern never matches")
	       | (_, FieldSort.Rec) =>
		     Error.error (region info, "pattern never matches"))
	  | derec' (RowPat (info, _, false), TupExp (_, _)) =
	    Error.error (region info, "pattern never matches")
	  | derec' (RowPat (info, patFields, true), TupExp (_, exps)) =
	    let
		val n = length exps
	    in
		if List.all (fn Field (_, Lab (_, label), _) =>
			     case Label.toInt label of
				 SOME i => i >= 1 andalso i <= n
			       | NONE => false) patFields
		then
		    raise Crash.Crash   (*--** *)
		    "SimplifyRec.derec': not implemented 1"
		else Error.error (region info, "pattern never matches")
	    end
	  | derec' (RowPat (info, patFields, false), RowExp (_, expFields)) =
	    let
		val (expFields', _) = FieldSort.sort expFields
	    in
		if length patFields = length expFields' then
		    ListPair.foldr
		    (fn (Field (_, Lab (_, s), pat),
			 Field (_, Lab (_, s'), exp), (cr, idsExpr)) =>
		     if s = s' then
			 let
			     val (cs, idsExps) = derec' (pat, exp)
			 in
			     (cs @ cr, idsExpr @ idsExpr)
			 end
		     else Error.error (region info, "pattern never matches"))
		    (nil, nil) (patFields, expFields')
		else Error.error (region info, "pattern never matches")
	    end
	  | derec' (RowPat (info, patFields, true), RowExp (_, expFields)) =
	    let
		val (expFields', _) = FieldSort.sort expFields
	    in
		if isSubarity (patFields, expFields') then
		    raise Crash.Crash   (*--** *)
		    "SimplifyRec.derec': not implemented 2"
		else Error.error (region info, "pattern never matches")
	    end
	  | derec' (VecPat (info, pats), VecExp (_, exps)) =
	    if length pats = length exps then
		ListPair.foldr (fn (pat, exp, (cr, idsExpr)) =>
				let
				    val (cs, idsExps) = derec' (pat, exp)
				in
				    (cs @ cr, idsExps @ idsExpr)
				end) (nil, nil) (pats, exps)
	    else Error.error (region info, "pattern never matches")
	  | derec' (pat as AsPat (_, _, _), exp) =
	    let
		val (ids, patOpt) = unalias pat
	    in
		case patOpt of
		    NONE =>
			(nil, [(ids, exp)])
		  | SOME pat' =>
			let
			    val (pat'', exp') = patToExp pat'
			    val (constraints, idsExpList) = derec' (pat'', exp)
			in
			    (constraints, (ids, exp')::idsExpList)
			end
	    end
	  | derec' (pat, _) =
	    Error.error (region (infoPat pat), "pattern never matches")

	fun unify (WildPat _, pat2) = (nil, pat2)
	  | unify (pat1, WildPat _) = (nil, pat1)
	  | unify (pat1 as LitPat (info, lit1), LitPat (_, lit2)) =
	    if lit1 = lit2 then (nil, pat1)   (*--** what about widths? *)
	    else Error.error (region info, "pattern never matches")
	  | unify (VarPat (info, id), pat2) = (nil, AsPat (info, id, pat2))
	  | unify (pat1, VarPat (info, id)) = (nil, AsPat (info, id, pat1))
	  | unify (pat1 as ConPat (_, longid, NONE),
		   ConPat (_, longid', NONE)) =
	    ([(longid, longid', false)], pat1)
	  | unify (ConPat (info, longid, SOME pat1),
		   ConPat (_, longid', SOME pat2)) =
	    let
		val (constraints, pat) = unify (pat1, pat2)
	    in
		((longid, longid', true)::constraints,
		 ConPat (info, longid, SOME pat))
	    end
	  | unify (RefPat (info, pat1), RefPat (_, pat2)) =
	    let
		val (constraints, pat) = unify (pat1, pat2)
	    in
		(constraints, RefPat (info, pat))
	    end
	  | unify (TupPat (info, pats1), TupPat (_, pats2)) =
	    if length pats1 = length pats2 then
		let
		    val (constraints, pats) =
			ListPair.foldr (fn (pat1, pat2, (cr, patr)) =>
					let
					    val (cs, pat) = unify (pat1, pat2)
					in
					    (cs @ cr, pat::patr)
					end) (nil, nil) (pats1, pats2)
		in
		    (constraints, TupPat (info, pats))
		end
	    else Error.error (region info, "pattern never matches")
	  | unify (TupPat (_, _), RowPat (_, _, true)) =
	    raise Crash.Crash "SimplifyRec.unify: not implemented 1"   (*--** *)
	  | unify (pat1 as RowPat (_, _, _), pat2 as TupPat (_, _)) =
	    unify (pat2, pat1)
	  | unify (RowPat (_, _, _), RowPat (_, _, _)) =
	    raise Crash.Crash "SimplifyRec.unify: not implemented 2"   (*--** *)
	  | unify (VecPat (info, pats1), VecPat (_, pats2)) =
	    if length pats1 = length pats2 then
		let
		    val (constraints, pats) =
			ListPair.foldr (fn (pat1, pat2, (cr, patr)) =>
					let
					    val (cs, pat) = unify (pat1, pat2)
					in
					    (cs @ cr, pat::patr)
					end) (nil, nil) (pats1, pats2)
		in
		    (constraints, VecPat (info, pats))
		end
	    else Error.error (region info, "pattern never matches")
	  | unify (AsPat (info, id, pat1), pat2) =
	    let
		val (constraints, pat) = unify (pat1, pat2)
	    in
		(constraints, AsPat (info, id, pat))
	    end
	  | unify (pat1, pat2 as AsPat (_, _, _)) = unify (pat2, pat1)
	  | unify (pat, _) =
	    Error.error (region (infoPat pat), "pattern never matches")

	fun preprocess (I.WildPat info) = (nil, WildPat info)
	  | preprocess (I.LitPat (info, lit)) = (nil, LitPat (info, lit))
	  | preprocess (I.VarPat (info, id)) = (nil, VarPat (info, id))
	  | preprocess (I.ConPat (info, longid, NONE, _)) =
	    (nil, ConPat (info, longid, NONE))
	  | preprocess (I.ConPat (info, longid, SOME pat, _)) =
	    let
		val (constraints, pat') = preprocess pat
	    in
		(constraints, ConPat (info, longid, SOME pat'))
	    end
	  | preprocess (I.RefPat (info, pat)) =
	    let
		val (constraints, pat') = preprocess pat
	    in
		(constraints, RefPat (info, pat'))
	    end
	  | preprocess (I.TupPat (info, pats)) =
	    let
		val (constraints, pats) =
		    List.foldr (fn (pat, (cr, patr)) =>
				let
				    val (cs, pat) = preprocess pat
				in
				    (cs @ cr, pat::patr)
				end) (nil, nil) pats
	    in
		(constraints, TupPat (info, pats))
	    end
	  | preprocess (I.RowPat (info, patFields)) =
	    let
		val hasDots = true   (*--** deduce from info type *)
		val (patFields', arity) = FieldSort.sort patFields
		val (constraints, patFields'') =
		    List.foldr (fn (Field (info, lab, pat), (cr, fieldr)) =>
				let
				    val (cs, pat') = preprocess pat
				in
				    (cs @ cr, Field (info, lab, pat')::fieldr)
				end) (nil, nil) patFields'
		val pat' =
		    case arity of
			FieldSort.Tup i =>
			    if hasDots then
				RowPat (info, patFields'', true)
			    else
				TupPat (info,
					List.map (fn Field (_, _, pat) => pat)
					patFields'')
		      | FieldSort.Rec =>
			    RowPat (info, patFields'', hasDots)
	    in
		(constraints, pat')
	    end
	  | preprocess (I.VecPat (info, pats)) =
	    let
		val (constraints, pats) =
		    List.foldr (fn (pat, (cr, patr)) =>
				let
				    val (cs, pat) = preprocess pat
				in
				    (cs @ cr, pat::patr)
				end) (nil, nil) pats
	    in
		(constraints, VecPat (info, pats))
	    end
	  | preprocess (I.AsPat (_, pat1, pat2)) =
	    let
		val (constraints1, pat1') = preprocess pat1
		val (constraints2, pat2') = preprocess pat2
		val (constraints3, pat') = unify (pat1', pat2')
	    in
		(constraints1 @ constraints2 @ constraints3, pat')
	    end
	  | preprocess (I.AltPat (info, _)) =
	    Error.error (region info,
			 "alternative pattern not allowed in val rec")
	  | preprocess (I.NegPat (info, _)) =
	    Error.error (region info,
			 "negated pattern not allowed in val rec")
	  | preprocess (I.GuardPat (info, _, _)) =
	    Error.error (region info,
			 "guard pattern not allowed in val rec")
	  | preprocess (I.WithPat (info, _, _)) =
	    Error.error (region info,
			 "with pattern not allowed in val rec")

	fun derec (ValDec (_, pat, exp)::decr) =
	    let
		val (constraints, pat') = preprocess pat
		val (constraints', idsExpList) = derec' (pat', exp)
		val (idExpList, aliases) =
		    List.foldr (fn ((ids, exp), (rest, subst)) =>
				let
				    val toId = List.hd ids
				in
				    ((toId, exp)::rest,
				     List.foldr
				     (fn (fromId, subst) =>
				      (fromId, toId)::subst)
				     subst (List.tl ids))
				end) (nil, nil) idsExpList
		val (constraints'', idExpList', aliases') = derec decr
	    in
		(constraints @ constraints' @ constraints'',
		 idExpList @ idExpList', aliases @ aliases')
	    end
	  | derec (RecDec (_, decs)::decr) =
	    let
		val (constraints, idExpList, aliases) = derec decs
		val (constraints', idExpList', aliases') = derec decr
	    in
		(constraints @ constraints',
		 idExpList @ idExpList', aliases @ aliases')
	    end
	  | derec nil = (nil, nil, nil)
    end
