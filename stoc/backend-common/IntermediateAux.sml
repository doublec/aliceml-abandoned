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

structure IntermediateAux :> INTERMEDIATE_AUX =
    struct
	structure Intermediate = IntermediateGrammar
	open Intermediate

	local
	    fun foldli' (x::xr, f, z, i) =
		foldli' (xr, f, f (i, x, z), i + 1)
	      | foldli' (nil, _, z, _) = z
	in
	    fun foldli f z xs = foldli' (xs, f, z, 1)
	end

	fun freshId coord = Id (coord, Stamp.new (), InId)

	fun labEq (Lab (_, s1), Lab (_, s2)) = s1 = s2

	fun idEq (Id (_, stamp1, _), Id (_, stamp2, _)) = stamp1 = stamp2

	fun longidEq (ShortId (_, id1), ShortId (_, id2)) = idEq (id1, id2)
	  | longidEq (LongId (_, longid1, lab1), LongId (_, longid2, lab2)) =
	    longidEq (longid1, longid2) andalso labEq (lab1, lab2)
	  | longidEq (_, _) = false

	fun lookup ((Id (_, stamp, _), id')::subst, id0 as Id (_, stamp0, _)) =
	    if stamp = stamp0 then id'
	    else lookup (subst, id0)
	  | lookup (nil, id0) = id0

	local
	    fun patternVariablesOf' (WildPat _, ids) = ids
	      | patternVariablesOf' (LitPat (_, _), ids) = ids
	      | patternVariablesOf' (VarPat (_, id), ids) = id::ids
	      | patternVariablesOf' (ConPat (_, _, NONE), ids) = ids
	      | patternVariablesOf' (ConPat (_, _, SOME pat), ids) =
		patternVariablesOf' (pat, ids)
	      | patternVariablesOf' (RefPat (_, pat), ids) =
		patternVariablesOf' (pat, ids)
	      | patternVariablesOf' (TupPat (_, pats), ids) =
		foldr patternVariablesOf' ids pats
	      | patternVariablesOf' (RowPat (_, fieldPats, _), ids) =
		foldr (fn (Field (_, _, pat), ids) =>
		       patternVariablesOf' (pat, ids)) ids fieldPats
	      | patternVariablesOf' (AsPat (_, pat1, pat2), ids) =
		patternVariablesOf' (pat1, patternVariablesOf' (pat2, ids))
	      | patternVariablesOf' (AltPat (_, pat::_), ids) =
		patternVariablesOf' (pat, ids)
	      | patternVariablesOf' (AltPat (_, nil), ids) = ids
	      | patternVariablesOf' (NegPat (_, _), ids) = ids
	      | patternVariablesOf' (GuardPat (_, pat, _), ids) =
		patternVariablesOf' (pat, ids)
	      | patternVariablesOf' (WithPat (_, pat, decs), ids) =
		patternVariablesOf' (pat, foldr declaredVariables ids decs)
	    and declaredVariables (ValDec (_, pat, _), ids) =
		patternVariablesOf' (pat, ids)
	      | declaredVariables (RecDec (_, decs), ids) =
		foldr declaredVariables ids decs
	      | declaredVariables (ConDec (_, id, _), ids) = id::ids
	in
	    fun patternVariablesOf pat = patternVariablesOf' (pat, nil)
	end

	type subst = (id * id) list

	fun substLongId (ShortId (coord, id), subst) =
	    ShortId (coord, lookup (subst, id))
	  | substLongId (longid as LongId (_, _, _), _) = longid

	fun substDecs (dec::decr, subst) =
	    substDec (dec, subst)::substDecs (decr, subst)
	  | substDecs (nil, _) = nil
	and substDec (ValDec (coord, pat, exp), subst) =
	    ValDec (coord, substPat (pat, subst), substExp (exp, subst))
	  | substDec (dec as ConDec (_, _, _), _) = dec
	  | substDec (RecDec (coord, decs), subst) =
	    RecDec (coord, List.map (fn dec => substDec (dec, subst)) decs)
	and substExp (exp as LitExp (_, _), _) = exp
	  | substExp (VarExp (coord, longid), subst) =
	    VarExp (coord, substLongId (longid, subst))
	  | substExp (exp as ConExp (_, _, _), _) = exp
	  | substExp (exp as RefExp _, _) = exp
	  | substExp (TupExp (coord, exps), subst) =
	    TupExp (coord, List.map (fn exp => substExp (exp, subst)) exps)
	  | substExp (RowExp (coord, expFields), subst) =
	    RowExp (coord,
		    List.map (fn Field (coord, lab, exp) =>
			      Field (coord, lab, substExp (exp, subst)))
		    expFields)
	  | substExp (exp as SelExp (_, _), _) = exp
	  | substExp (FunExp (coord, id, exp), subst) =
	    FunExp (coord, id, substExp (exp, subst))
	  | substExp (AppExp (coord, exp1, exp2), subst) =
	    AppExp (coord, substExp (exp1, subst), substExp (exp2, subst))
	  | substExp (AdjExp (coord, exp1, exp2), subst) =
	    AdjExp (coord, substExp (exp1, subst), substExp (exp2, subst))
	  | substExp (AndExp (coord, exp1, exp2), subst) =
	    AndExp (coord, substExp (exp1, subst), substExp (exp2, subst))
	  | substExp (OrExp (coord, exp1, exp2), subst) =
	    OrExp (coord, substExp (exp1, subst), substExp (exp2, subst))
	  | substExp (IfExp (coord, exp1, exp2, exp3), subst) =
	    IfExp (coord, substExp (exp1, subst),
		   substExp (exp2, subst), substExp (exp3, subst))
	  | substExp (WhileExp (coord, exp1, exp2), subst) =
	    WhileExp (coord, substExp (exp1, subst), substExp (exp2, subst))
	  | substExp (SeqExp (coord, exps), subst) =
	    SeqExp (coord, List.map (fn exp => substExp (exp, subst)) exps)
	  | substExp (CaseExp (coord, exp, matches), subst) =
	    CaseExp (coord, substExp (exp, subst),
		     substMatches (matches, subst))
	  | substExp (RaiseExp (coord, exp), subst) =
	    RaiseExp (coord, substExp (exp, subst))
	  | substExp (HandleExp (coord, exp, matches), subst) =
	    HandleExp (coord, substExp (exp, subst),
		       substMatches (matches, subst))
	  | substExp (LetExp (coord, decs, exp), subst) =
	    LetExp (coord, substDecs (decs, subst), substExp (exp, subst))
	and substMatches (matches, subst) =
	    List.map (fn Match (coord, pat, exp) =>
		      Match (coord, substPat (pat, subst),
			     substExp (exp, subst))) matches
	and substPat (pat as WildPat _, _) = pat
	  | substPat (pat as LitPat (_, _), _) = pat
	  | substPat (pat as VarPat (_, _), _) = pat
	  | substPat (ConPat (coord, longid, NONE), subst) =
	    ConPat (coord, substLongId (longid, subst), NONE)
	  | substPat (ConPat (coord, longid, SOME pat), subst) =
	    ConPat (coord, substLongId (longid, subst),
		    SOME (substPat (pat, subst)))
	  | substPat (RefPat (coord, pat), subst) =
	    RefPat (coord, substPat (pat, subst))
	  | substPat (TupPat (coord, pats), subst) =
	    TupPat (coord, List.map (fn pat => substPat (pat, subst)) pats)
	  | substPat (RowPat (coord, patFields, hasDots), subst) =
	    RowPat (coord,
		    List.map (fn Field (coord, lab, pat) =>
			      Field (coord, lab, substPat (pat, subst)))
		    patFields, hasDots)
	  | substPat (AsPat (coord, pat1, pat2), subst) =
	    AsPat (coord, substPat (pat1, subst), substPat (pat2, subst))
	  | substPat (AltPat (coord, pats), subst) =
	    AltPat (coord, List.map (fn pat => substPat (pat, subst)) pats)
	  | substPat (NegPat (coord, pat), subst) =
	    NegPat (coord, substPat (pat, subst))
	  | substPat (GuardPat (coord, pat, exp), subst) =
	    GuardPat (coord, substPat (pat, subst), substExp (exp, subst))
	  | substPat (WithPat (coord, pat, decs), subst) =
	    WithPat (coord, substPat (pat, subst), substDecs (decs, subst))

	(* If the same test occurs in two patterns at the same position,
	 * then these may be merged by the pattern matching compiler.
	 * In this process, a global substitution is built such that the
	 * identifiers bound at each pattern position are all mapped to
	 * common identifiers.
	 * In the presence of disjunctive patterns, such a substitution can
	 * in general only be made consistent with all pattern bindings by
	 * first uniquely renaming, then binding all the original identifiers
	 * by `with' declarations.  `with' declarations are not affected by
	 * the substitution because they are never merged.
	 *
	 * `separateAlt' moves all bindings to `with' declarations.  These
	 * are placed right at the end of each alternative pattern to allow
	 * for a maximum of merging possibilities.
	 * In principle, it is sufficient to do so only within disjunctive
	 * patterns.  If we apply this on the toplevel as well however,
	 * we need not substitute into the right hand side of a match.
	 *)

	fun separateAlt pat =
	    let
		val (pat', subst) = relax (pat, nil)
		val decs =
		    List.foldl
		    (fn ((id, id'), decs) =>
		     let
			 val coord = infoId id
			 val exp = VarExp (coord, ShortId (coord, id'))
		     in
			 ValDec (coord, VarPat (coord, id), exp)::decs
		     end) nil subst
	    in
		WithPat (infoPat pat', pat', decs)
	    end
	and relax (pat as WildPat _, subst) = (pat, subst)
	  | relax (pat as LitPat (_, _), subst) = (pat, subst)
	  | relax (VarPat (coord, id), subst) =
	    let
		val id' = freshId coord
	    in
		(VarPat (coord, id'), (id, id')::subst)
	    end
	  | relax (pat as ConPat (_, _, NONE), subst) = (pat, subst)
	  | relax (ConPat (coord, longid, SOME pat), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
	    in
		(ConPat (coord, longid, SOME pat'), subst')
	    end
	  | relax (RefPat (coord, pat), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
	    in
		(RefPat (coord, pat'), subst')
	    end
	  | relax (TupPat (coord, pats), subst) =
	    let
		val (pats', subst') =
		    List.foldr (fn (pat, (pats, subst)) =>
				let
				    val (pat', subst') = relax (pat, subst)
				in
				    (pat'::pats, subst')
				end) (nil, subst) pats
	    in
		(TupPat (coord, pats'), subst')
	    end
	  | relax (RowPat (coord, patFields, hasDots), subst) =
	    let
		val (patFields', subst') =
		    List.foldr
		    (fn (Field (coord, lab, pat), (patFields, subst)) =>
		     let
			 val (pat', subst') = relax (pat, subst)
		     in
			 (Field (coord, lab, pat')::patFields, subst')
		     end) (nil, subst) patFields
	    in
		(RowPat (coord, patFields', hasDots), subst')
	    end
	  | relax (AsPat (coord, pat1, pat2), subst) =
	    let
		val (pat1', subst') = relax (pat1, subst)
		val (pat2', subst'') = relax (pat2, subst')
	    in
		(AsPat (coord, pat1', pat2'), subst'')
	    end
	  | relax (AltPat (coord, pats), subst) =
	    (AltPat (coord, List.map separateAlt pats), subst)
	  | relax (NegPat (coord, pat), subst) =
	    (NegPat (coord, separateAlt pat), subst)
	  | relax (GuardPat (coord, pat, exp), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
	    in
		(GuardPat (coord, pat', substExp (exp, subst')), subst')
	    end
	  | relax (WithPat (coord, pat, decs), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
	    in
		(WithPat (coord, pat', substDecs (decs, subst')), subst')
	    end
    end
