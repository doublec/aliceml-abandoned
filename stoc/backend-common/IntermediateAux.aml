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

	fun idEq (Id (_, stamp1, _), Id (_, stamp2, _)) = stamp1 = stamp2

	fun occursInDec (ValDec (_, pat, exp), id) =
	    occursInPat (pat, id) orelse occursInExp (exp, id)
	  | occursInDec (ConDec (_, _, _), _) = false
	  | occursInDec (RecDec (_, decs), id) =
	    List.exists (fn dec => occursInDec (dec, id)) decs
	and occursInExp (LitExp (_, _), _) = false
	  | occursInExp (PrimExp (_, _), _) = false
	  | occursInExp (VarExp (_, ShortId (_, id)), id') = idEq (id, id')
	  | occursInExp (VarExp (_, LongId (_, _, _)), _) = false
	  | occursInExp (ConExp (_, _, _), _) = false
	  | occursInExp (RefExp _, _) = false
	  | occursInExp (TupExp (_, exps), id) =
	    List.exists (fn exp => occursInExp (exp, id)) exps
	  | occursInExp (RowExp (_, expFields), id) =
	    List.exists (fn Field (_, _, exp) => occursInExp (exp, id))
	    expFields
	  | occursInExp (SelExp (_, _), _) = false
	  | occursInExp (VecExp (_, exps), id) =
	    List.exists (fn exp => occursInExp (exp, id)) exps
	  | occursInExp (FunExp (_, _, exp), id) = occursInExp (exp, id)
	  | occursInExp (AppExp (_, exp1, exp2), id) =
	    occursInExp (exp1, id) orelse occursInExp (exp2, id)
	  | occursInExp (AdjExp (_, exp1, exp2), id) =
	    occursInExp (exp1, id) orelse occursInExp (exp2, id)
	  | occursInExp (AndExp (_, exp1, exp2), id) =
	    occursInExp (exp1, id) orelse occursInExp (exp2, id)
	  | occursInExp (OrExp (_, exp1, exp2), id) =
	    occursInExp (exp1, id) orelse occursInExp (exp2, id)
	  | occursInExp (IfExp (_, exp1, exp2, exp3), id) =
	    occursInExp (exp1, id) orelse occursInExp (exp2, id) orelse
	    occursInExp (exp3, id)
	  | occursInExp (WhileExp (_, exp1, exp2), id) =
	    occursInExp (exp1, id) orelse occursInExp (exp2, id)
	  | occursInExp (SeqExp (_, exps), id) =
	    List.exists (fn exp => occursInExp (exp, id)) exps
	  | occursInExp (CaseExp (_, exp, matches), id) =
	    occursInExp (exp, id) orelse occursInMatches (matches, id)
	  | occursInExp (RaiseExp (_, exp), id) = occursInExp (exp, id)
	  | occursInExp (HandleExp (_, exp, matches), id) =
	    occursInExp (exp, id) orelse occursInMatches (matches, id)
	  | occursInExp (LetExp (_, decs, exp), id) =
	    List.exists (fn dec => occursInDec (dec, id)) decs orelse
	    occursInExp (exp, id)
	and occursInMatches (matches, id) =
	    List.exists (fn Match (_, pat, exp) =>
		       occursInPat (pat, id) orelse occursInExp (exp, id))
	    matches
	and occursInPat (WildPat _, _) = false
	  | occursInPat (LitPat (_, _), _) = false
	  | occursInPat (VarPat (_, _), _) = false
	  | occursInPat (ConPat (_, _, NONE), _) = false
	  | occursInPat (ConPat (_, _, SOME pat), id) = occursInPat (pat, id)
	  | occursInPat (RefPat (_, pat), id) = occursInPat (pat, id)
	  | occursInPat (TupPat (_, pats), id) =
	    List.exists (fn pat => occursInPat (pat, id)) pats
	  | occursInPat (RowPat (_, patFields, _), id) =
	    List.exists (fn Field (_, _, pat) => occursInPat (pat, id))
	    patFields
	  | occursInPat (VecPat (_, pats), id) =
	    List.exists (fn pat => occursInPat (pat, id)) pats
	  | occursInPat (AsPat (_, pat1, pat2), id) =
	    occursInPat (pat1, id) orelse occursInPat (pat2, id)
	  | occursInPat (AltPat (_, pats), id) =
	    List.exists (fn pat => occursInPat (pat, id)) pats
	  | occursInPat (NegPat (_, pat), id) = occursInPat (pat, id)
	  | occursInPat (GuardPat (_, pat, exp), id) =
	    occursInPat (pat, id) orelse occursInExp (exp, id)
	  | occursInPat (WithPat (_, pat, decs), id) =
	    occursInPat (pat, id) orelse
	    List.exists (fn dec => occursInDec (dec, id)) decs

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
	      | patternVariablesOf' (VecPat (_, pats), ids) =
		foldr patternVariablesOf' ids pats
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
	      | declaredVariables (ConDec (_, id, _), ids) = id::ids
	      | declaredVariables (RecDec (_, decs), ids) =
		foldr declaredVariables ids decs
	in
	    fun patternVariablesOf pat = patternVariablesOf' (pat, nil)
	end

	type subst = (id * id) list

	fun lookup ((Id (_, stamp, _), id')::subst, id0 as Id (_, stamp0, _)) =
	    if stamp = stamp0 then id'
	    else lookup (subst, id0)
	  | lookup (nil, id0) = id0

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
	  | substExp (exp as PrimExp (_, _), _) = exp
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
	  | substExp (VecExp (coord, exps), subst) =
	    VecExp (coord, List.map (fn exp => substExp (exp, subst)) exps)
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
	  | substPat (VecPat (coord, pats), subst) =
	    VecPat (coord, List.map (fn pat => substPat (pat, subst)) pats)
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
		    List.map
		    (fn (id, id') =>
		     let
			 val coord = infoId id
			 val exp = VarExp (coord, ShortId (coord, id'))
		     in
			 ValDec (coord, VarPat (coord, id), exp)
		     end) subst
	    in
		case decs of
		    nil => pat'
		  | _::_ => WithPat (infoPat pat', pat', decs)
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
	  | relax (VecPat (coord, pats), subst) =
	    let
		val (pats', subst') =
		    List.foldr (fn (pat, (pats, subst)) =>
				let
				    val (pat', subst') = relax (pat, subst)
				in
				    (pat'::pats, subst')
				end) (nil, subst) pats
	    in
		(VecPat (coord, pats'), subst')
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
