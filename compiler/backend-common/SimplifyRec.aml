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
	open IntermediateAux

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

	fun unify (pat1 as WildPat _, _) = pat1
	  | unify (_, pat2 as WildPat _) = pat2
	  | unify (pat1 as LitPat (coord, lit1), LitPat (_, lit2)) =
	    if lit1 = lit2 then pat1
	    else Error.error (coord, "pattern never matches")
	  | unify (pat1 as VarPat (coord, _), pat2) =
	    AsPat (coord, pat1, pat2)
	  | unify (pat1, pat2 as VarPat (coord, _)) =
	    AsPat (coord, pat1, pat2)
	  | unify (pat1 as ConPat (coord, longid, NONE),
		   ConPat (_, longid', NONE)) =
	    if longidEq (longid, longid') then pat1   (*--** too restrictive *)
	    else Error.error (coord, "pattern never matches")
	  | unify (ConPat (coord, longid, SOME pat1'),
		   ConPat (_, longid', SOME pat2')) =
	    if longidEq (longid, longid') then   (*--** too restrictive *)
		ConPat (coord, longid, SOME (unify (pat1', pat2')))
	    else Error.error (coord, "pattern never matches")
	  | unify (RefPat (coord, pat1), RefPat (_, pat2)) =
	    RefPat (coord, unify (pat1, pat2))
	  | unify (TupPat (coord, pats1), TupPat (_, pats2)) =
	    if length pats1 = length pats2 then
		TupPat (coord, ListPair.map unify (pats1, pats2))
	    else Error.error (coord, "pattern never matches")
	  | unify (TupPat (coord, pats), RowPat (_, fieldPats, hasDots)) =
	    (case FieldSort.sort fieldPats of
		 (_, FieldSort.Tup i) =>
		     if i = length pats then
			 TupPat (coord,
				 ListPair.map (fn (pat1, Field (_, _, pat2)) =>
					       unify (pat1, pat2))
				 (pats, fieldPats))
		     else Error.error (coord, "pattern never matches")
	       | (_, FieldSort.Rec) =>
		     if hasDots then
			 Crash.crash
			 "MatchCompilationPhase.unify: not yet implemented 1"
		     else Error.error (coord, "pattern never matches"))
	  | unify (pat1 as RowPat (_, _, _), pat2 as TupPat (_, _)) =
	    unify (pat2, pat1)
	  | unify (RowPat (coord, fieldPats1, false),
		   RowPat (_, fieldPats2, false)) =
	    let
		val (fieldPats1', _) = FieldSort.sort fieldPats1
		val (fieldPats2', _) = FieldSort.sort fieldPats2
	    in
		Crash.crash
		"MatchCompilationPhase.unify: not yet implemented 2"
	    end
	  | unify (RowPat (coord, fieldPats1, true),
		   RowPat (_, fieldPats2, false)) =
	    Crash.crash "MatchCompilationPhase.unify: not yet implemented 3"
	  | unify (pat1 as RowPat (_, _, false), pat2 as RowPat (_, _, true)) =
	    unify (pat2, pat1)
	  | unify (RowPat (coord, fieldPats1, true),
		   RowPat (_, fieldPats2, true)) =
	    Crash.crash "MatchCompilationPhase.unify: not yet implemented 4"
	  | unify (AsPat (_, pat1, pat2), pat3) =
	    unify (unify (pat1, pat2), pat3)
	  | unify (AltPat (coord, _), _) =
	    Error.error (coord, "alternative pattern not allowed in let rec")
	  | unify (_, AltPat (coord, _)) =
	    Error.error (coord, "alternative pattern not allowed in let rec")
	  | unify (NegPat (coord, _), _) =
	    Error.error (coord, "negated pattern not allowed in let rec")
	  | unify (_, NegPat (coord, _)) =
	    Error.error (coord, "negated pattern not allowed in let rec")
	  | unify (GuardPat (coord, _, _), _) =
	    Error.error (coord, "guard pattern not allowed in let rec")
	  | unify (_, GuardPat (coord, _, _)) =
	    Error.error (coord, "guard pattern not allowed in let rec")
	  | unify (WithPat (coord, _, _), _) =
	    Error.error (coord, "with pattern not allowed in let rec")
	  | unify (_, WithPat (coord, _, _)) =
	    Error.error (coord, "with pattern not allowed in let rec")
	  | unify (_, pat) =
	    Error.error (infoPat pat, "pattern never matches")

	fun tame (WildPat coord) = VarPat (coord, freshId coord)
	  | tame (pat as LitPat (_, _)) = pat
	  | tame (pat as VarPat (_, _)) = pat
	  | tame (pat as ConPat (_, _, NONE)) = pat
	  | tame (ConPat (coord, longid, SOME pat)) =
	    ConPat (coord, longid, SOME (tame pat))
	  | tame (RefPat (coord, pat)) = RefPat (coord, tame pat)
	  | tame (TupPat (coord, pats)) = TupPat (coord, List.map tame pats)
	  | tame (RowPat (coord, patFields, hasDots)) =
	    RowPat (coord,
		    List.map (fn Field (coord, lab, pat) =>
			      Field (coord, lab, tame pat)) patFields, hasDots)
	  | tame (pat as AsPat (coord, VarPat (_, _), _)) = pat
	  | tame (pat as AsPat (coord, _, VarPat (_, _))) = pat
	  | tame (AsPat (coord, pat1, pat2)) = tame (unify (pat1, pat2))
	  | tame (AltPat (coord, _)) =
	    Error.error (coord, "alternative pattern not allowed in let rec")
	  | tame (NegPat (coord, _)) =
	    Error.error (coord, "negated pattern not allowed in let rec")
	  | tame (GuardPat (coord, _, _)) =
	    Error.error (coord, "guard pattern not allowed in let rec")
	  | tame (WithPat (coord, _, _)) =
	    Error.error (coord, "with pattern not allowed in let rec")

	fun patToExp (WildPat _) = Crash.crash "MatchCompilationPhase.patToExp"
	  | patToExp (LitPat (coord, lit)) = LitExp (coord, lit)
	  | patToExp (VarPat (coord, id)) = VarExp (coord, ShortId (coord, id))
	  | patToExp (ConPat (coord, longid, NONE)) =
	    ConExp (coord, longid, false)
	  | patToExp (ConPat (coord, longid, SOME pat)) =
	    AppExp (coord, ConExp (coord, longid, true), patToExp pat)
	  | patToExp (RefPat (coord, pat)) =
	    AppExp (coord, RefExp coord, patToExp pat)
	  | patToExp (TupPat (coord, pats)) =
	    TupExp (coord, List.map patToExp pats)
	  | patToExp (RowPat (coord, patFields, hasDots)) =
	    (*--** record patterns with dots must be resolved using the rhs *)
	    Crash.crash "MatchCompilationPhase.patToExp"
	  | patToExp (AsPat (coord, pat as VarPat (_, _), _)) = patToExp pat
	  | patToExp (AsPat (coord, _, pat as VarPat (_, _))) = patToExp pat
	  | patToExp _ = Crash.crash "MatchCompilationPhase.patToExp"

	fun derec (WildPat _, exp) = [(nil, exp)]
	  | derec (LitPat (coord, lit1), LitExp (_, lit2)) =
	    if lit1 = lit2 then nil
	    else Error.error (coord, "pattern never matches")
	  | derec (VarPat (_, id), exp) = [([id], exp)]
	  | derec (ConPat (coord, longid1, NONE), ConExp (_, longid2, false)) =
	    if longidEq (longid1, longid2) then nil   (*--** too restrictive *)
	    else Error.error (coord, "pattern never matches")
	  | derec (ConPat (coord, longid1, SOME pat),
		   AppExp (_, ConExp (_, longid2, true), exp)) =
	    if longidEq (longid1, longid2) then derec (pat, exp)
	    else Error.error (coord, "pattern never matches")
	  | derec (RefPat (_, pat), AppExp (_, RefExp _, exp)) =
	    derec (pat, exp)
	  | derec (TupPat (coord, pats), TupExp (_, exps)) =
	    if length pats = length exps then
		ListPair.foldr (fn (pat, exp, rest) =>
				derec (pat, exp) @ rest) nil (pats, exps)
	    else Error.error (coord, "pattern never matches")
	  | derec (TupPat (coord, pats), RowExp (_, expFields)) =
	    (case FieldSort.sort expFields of
		 (_, FieldSort.Tup n) =>
		     if length pats = n then
			 List.foldr (fn (Field (_, Lab (_, s), exp), rest) =>
				     let
					 val i = valOf (Int.fromString s) - 1
				     in
					 derec (List.nth (pats, i), exp) @ rest
				     end) nil expFields
		     else Error.error (coord, "pattern never matches")
	       | (_, FieldSort.Rec) =>
		     Error.error (coord, "pattern never matches"))
	  | derec (RowPat (coord, patFields, hasDots), TupExp (_, exps)) =
	    let
		val (patFields', arity) = FieldSort.sort patFields
		val n = length exps
	    in
		if hasDots then
		    if List.all (fn Field (_, Lab (_, s), _) =>
				 case Int.fromString s of
				     SOME i => i >= 1 andalso i <= n
				   | NONE => false) patFields'
		    then
			Crash.crash   (*--** *)
			"MatchCompilationPhase.derec: not implemented 1"
		    else Error.error (coord, "pattern never matches")
		else
		    case arity of
			FieldSort.Tup i =>
			    if i = n then
				ListPair.foldr
				(fn (Field (_, _, pat), exp, rest) =>
				 derec (pat, exp) @ rest) nil
				(patFields', exps)
			    else Error.error (coord, "pattern never matches")
		      | FieldSort.Rec =>
			    Error.error (coord, "pattern never matches")
	    end
	  | derec (RowPat (coord, patFields, hasDots), RowExp (_, expFields)) =
	    let
		val (patFields', _) = FieldSort.sort patFields
		val (expFields', _) = FieldSort.sort expFields
	    in
		if hasDots then
		    if isSubarity (patFields', expFields') then
			Crash.crash   (*--** *)
			"MatchCompilationPhase.derec: not implemented 2"
		    else Error.error (coord, "pattern never matches")
		else
		    if ListPair.all (fn (Field (_, Lab (_, s), _),
					 Field (_, Lab (_, s'), _)) => s = s')
			(patFields', expFields')
		    then
			ListPair.foldr (fn (Field (_, _, pat),
					    Field (_, _, exp), rest) =>
					derec (pat, exp) @ rest) nil
			(patFields', expFields')
		    else Error.error (coord, "pattern never matches")
	    end
	  | derec (AsPat (_, VarPat (_, id), pat), exp) =
	    let
		val pat' = tame pat
	    in
		([id], patToExp pat')::derec (pat', exp)
	    end
	  | derec (AsPat (_, pat, VarPat (_, id)), exp) =
	    let
		val pat' = tame pat
	    in
		([id], patToExp pat')::derec (pat', exp)
	    end
	  | derec (AsPat (_, pat1, pat2), exp) =
	    derec (unify (pat1, pat2), exp)
	  | derec (AltPat (coord, _), _) =
	    Error.error (coord, "alternative pattern not allowed in let rec")
	  | derec (NegPat (coord, _), _) =
	    Error.error (coord, "negated pattern not allowed in let rec")
	  | derec (GuardPat (coord, _, _), _) =
	    Error.error (coord, "guard pattern not allowed in let rec")
	  | derec (WithPat (coord, _, _), _) =
	    Error.error (coord, "with pattern not allowed in let rec")
	  | derec (pat, _) =
	    Error.error (infoPat pat, "pattern never matches")
    end
