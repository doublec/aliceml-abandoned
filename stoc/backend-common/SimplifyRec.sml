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

	fun infoPat (WildPat coord) = coord
	  | infoPat (LitPat (coord, _)) = coord
	  | infoPat (VarPat (coord, _)) = coord
	  | infoPat (ConPat (coord, _, _)) = coord
	  | infoPat (RefPat (coord, _)) = coord
	  | infoPat (TupPat (coord, _)) = coord
	  | infoPat (RowPat (coord, _, _)) = coord
	  | infoPat (VecPat (coord, _)) = coord
	  | infoPat (AsPat (coord, _, _)) = coord

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

	fun patToExp (WildPat coord) =
	    let
		val id = IntermediateAux.freshId coord
	    in
		(VarPat (coord, id), VarExp (coord, ShortId (coord, id)))
	    end
	  | patToExp (pat as LitPat (coord, lit)) = (pat, LitExp (coord, lit))
	  | patToExp (pat as VarPat (coord, id)) =
	    (pat, VarExp (coord, ShortId (coord, id)))
	  | patToExp (pat as ConPat (coord, longid, NONE)) =
	    (pat, ConExp (coord, longid, false))
	  | patToExp (ConPat (coord, longid, SOME pat)) =
	    let
		val (pat', exp') = patToExp pat
	    in
		(ConPat (coord, longid, SOME pat'),
		 AppExp (coord, ConExp (coord, longid, true), exp'))
	    end
	  | patToExp (RefPat (coord, pat)) =
	    let
		val (pat', exp') = patToExp pat
	    in
		(RefPat (coord, pat'), AppExp (coord, RefExp coord, exp'))
	    end
	  | patToExp (TupPat (coord, pats)) =
	    let
		val (pats', exps') = ListPair.unzip (List.map patToExp pats)
	    in
		(TupPat (coord, pats'), TupExp (coord, exps'))
	    end
	  | patToExp (RowPat (coord, patFields, hasDots)) =
	    (*--** record patterns with dots must be resolved using the rhs *)
	    Crash.crash "SimplifyRec.patToExp"
	  | patToExp (VecPat (coord, pats)) =
	    let
		val (pats', exps') = ListPair.unzip (List.map patToExp pats)
	    in
		(VecPat (coord, pats'), VecExp (coord, exps'))
	    end
	  | patToExp (pat as AsPat (coord, id, _)) =
	    (pat, VarExp (coord, ShortId (coord, id)))

	fun derec' (WildPat _, exp) = (nil, [(nil, exp)])
	  | derec' (LitPat (coord, lit1), LitExp (_, lit2)) =
	    if lit1 = lit2 then (nil, nil)
	    else Error.error (coord, "pattern never matches")
	  | derec' (VarPat (_, id), exp) = (nil, [([id], exp)])
	  | derec' (ConPat (coord, longid1, NONE),
		    ConExp (_, longid2, false)) =
	    ([(longid1, longid2, false)], nil)
	  | derec' (ConPat (coord, longid1, SOME pat),
		   AppExp (_, ConExp (_, longid2, true), exp)) =
	    let
		val (constraints, idsExpList) = derec' (pat, exp)
	    in
		((longid1, longid2, true)::constraints, idsExpList)
	    end
	  | derec' (RefPat (_, pat), AppExp (_, RefExp _, exp)) =
	    derec' (pat, exp)
	  | derec' (TupPat (coord, pats), TupExp (_, exps)) =
	    if length pats = length exps then
		ListPair.foldr (fn (pat, exp, (cr, idsExpr)) =>
				let
				    val (cs, idsExps) = derec' (pat, exp)
				in
				    (cs @ cr, idsExps @ idsExpr)
				end) (nil, nil) (pats, exps)
	    else Error.error (coord, "pattern never matches")
	  | derec' (TupPat (coord, pats), RowExp (_, expFields)) =
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
		     else Error.error (coord, "pattern never matches")
	       | (_, FieldSort.Rec) =>
		     Error.error (coord, "pattern never matches"))
	  | derec' (RowPat (coord, _, false), TupExp (_, _)) =
	    Error.error (coord, "pattern never matches")
	  | derec' (RowPat (coord, patFields, true), TupExp (_, exps)) =
	    let
		val n = length exps
	    in
		if List.all (fn Field (_, Lab (_, s), _) =>
			     case Int.fromString s of
				 SOME i => i >= 1 andalso i <= n
			       | NONE => false) patFields
		then
		    Crash.crash   (*--** *)
		    "SimplifyRec.derec': not implemented 1"
		else Error.error (coord, "pattern never matches")
	    end
	  | derec' (RowPat (coord, patFields, false), RowExp (_, expFields)) =
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
		     else Error.error (coord, "pattern never matches"))
		    (nil, nil) (patFields, expFields')
		else Error.error (coord, "pattern never matches")
	    end
	  | derec' (RowPat (coord, patFields, true), RowExp (_, expFields)) =
	    let
		val (expFields', _) = FieldSort.sort expFields
	    in
		if isSubarity (patFields, expFields') then
		    Crash.crash   (*--** *)
		    "SimplifyRec.derec': not implemented 2"
		else Error.error (coord, "pattern never matches")
	    end
	  | derec' (VecPat (coord, pats), VecExp (_, exps)) =
	    if length pats = length exps then
		ListPair.foldr (fn (pat, exp, (cr, idsExpr)) =>
				let
				    val (cs, idsExps) = derec' (pat, exp)
				in
				    (cs @ cr, idsExps @ idsExpr)
				end) (nil, nil) (pats, exps)
	    else Error.error (coord, "pattern never matches")
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
	    Error.error (infoPat pat, "pattern never matches")

	fun unify (WildPat _, pat2) = (nil, pat2)
	  | unify (pat1, WildPat _) = (nil, pat1)
	  | unify (pat1 as LitPat (coord, lit1), LitPat (_, lit2)) =
	    if lit1 = lit2 then (nil, pat1)   (*--** what about widths? *)
	    else Error.error (coord, "pattern never matches")
	  | unify (VarPat (coord, id), pat2) = (nil, AsPat (coord, id, pat2))
	  | unify (pat1, VarPat (coord, id)) = (nil, AsPat (coord, id, pat1))
	  | unify (pat1 as ConPat (coord, longid, NONE),
		   ConPat (_, longid', NONE)) =
	    ([(longid, longid', false)], pat1)
	  | unify (ConPat (coord, longid, SOME pat1),
		   ConPat (_, longid', SOME pat2)) =
	    let
		val (constraints, pat) = unify (pat1, pat2)
	    in
		((longid, longid', true)::constraints,
		 ConPat (coord, longid, SOME pat))
	    end
	  | unify (RefPat (coord, pat1), RefPat (_, pat2)) =
	    let
		val (constraints, pat) = unify (pat1, pat2)
	    in
		(constraints, RefPat (coord, pat))
	    end
	  | unify (TupPat (coord, pats1), TupPat (_, pats2)) =
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
		    (constraints, TupPat (coord, pats))
		end
	    else Error.error (coord, "pattern never matches")
	  | unify (TupPat (_, _), RowPat (_, _, true)) =
	    Crash.crash "SimplifyRec.unify: not implemented 1"   (*--** *)
	  | unify (pat1 as RowPat (_, _, _), pat2 as TupPat (_, _)) =
	    unify (pat2, pat1)
	  | unify (RowPat (_, _, _), RowPat (_, _, _)) =
	    Crash.crash "SimplifyRec.unify: not implemented 2"   (*--** *)
	  | unify (VecPat (coord, pats1), VecPat (_, pats2)) =
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
		    (constraints, VecPat (coord, pats))
		end
	    else Error.error (coord, "pattern never matches")
	  | unify (AsPat (coord, id, pat1), pat2) =
	    let
		val (constraints, pat) = unify (pat1, pat2)
	    in
		(constraints, AsPat (coord, id, pat))
	    end
	  | unify (pat1, pat2 as AsPat (_, _, _)) = unify (pat2, pat1)
	  | unify (pat, _) =
	    Error.error (infoPat pat, "pattern never matches")

	fun preprocess (I.WildPat coord) = (nil, WildPat coord)
	  | preprocess (I.LitPat (coord, lit)) = (nil, LitPat (coord, lit))
	  | preprocess (I.VarPat (coord, id)) = (nil, VarPat (coord, id))
	  | preprocess (I.ConPat (coord, longid, NONE)) =
	    (nil, ConPat (coord, longid, NONE))
	  | preprocess (I.ConPat (coord, longid, SOME pat)) =
	    let
		val (constraints, pat') = preprocess pat
	    in
		(constraints, ConPat (coord, longid, SOME pat'))
	    end
	  | preprocess (I.RefPat (coord, pat)) =
	    let
		val (constraints, pat') = preprocess pat
	    in
		(constraints, RefPat (coord, pat'))
	    end
	  | preprocess (I.TupPat (coord, pats)) =
	    let
		val (constraints, pats) =
		    List.foldr (fn (pat, (cr, patr)) =>
				let
				    val (cs, pat) = preprocess pat
				in
				    (cs @ cr, pat::patr)
				end) (nil, nil) pats
	    in
		(constraints, TupPat (coord, pats))
	    end
	  | preprocess (I.RowPat (coord, patFields, hasDots)) =
	    let
		val (patFields', arity) = FieldSort.sort patFields
		val (constraints, patFields'') =
		    List.foldr (fn (Field (coord, lab, pat), (cr, fieldr)) =>
				let
				    val (cs, pat') = preprocess pat
				in
				    (cs @ cr, Field (coord, lab, pat')::fieldr)
				end) (nil, nil) patFields'
		val pat' =
		    case arity of
			FieldSort.Tup i =>
			    if hasDots then
				RowPat (coord, patFields'', true)
			    else
				TupPat (coord,
					List.map (fn Field (_, _, pat) => pat)
					patFields'')
		      | FieldSort.Rec =>
			    RowPat (coord, patFields'', hasDots)
	    in
		(constraints, pat')
	    end
	  | preprocess (I.VecPat (coord, pats)) =
	    let
		val (constraints, pats) =
		    List.foldr (fn (pat, (cr, patr)) =>
				let
				    val (cs, pat) = preprocess pat
				in
				    (cs @ cr, pat::patr)
				end) (nil, nil) pats
	    in
		(constraints, VecPat (coord, pats))
	    end
	  | preprocess (I.AsPat (coord, pat1, pat2)) =
	    let
		val (constraints1, pat1') = preprocess pat1
		val (constraints2, pat2') = preprocess pat2
		val (constraints3, pat') = unify (pat1', pat2')
	    in
		(constraints1 @ constraints2 @ constraints3, pat')
	    end
	  | preprocess (I.AltPat (coord, _)) =
	    Error.error (coord, "alternative pattern not allowed in val rec")
	  | preprocess (I.NegPat (coord, _)) =
	    Error.error (coord, "negated pattern not allowed in val rec")
	  | preprocess (I.GuardPat (coord, _, _)) =
	    Error.error (coord, "guard pattern not allowed in val rec")
	  | preprocess (I.WithPat (coord, _, _)) =
	    Error.error (coord, "with pattern not allowed in val rec")

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
