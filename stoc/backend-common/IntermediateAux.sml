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

structure IntermediateAux :> INTERMEDIATE_AUX =
    struct
	structure I = IntermediateGrammar
	structure O = FlatGrammar

	open I

	fun id_info {region, typ = _} = {region = region}

	fun freshIntermediateId info = Id (info, Stamp.new (), Name.InId)

	type subst = (Stamp.t * Stamp.t) list

	fun lookup ((stamp, stamp')::subst, id0 as Id (info, stamp0, name)) =
	    if stamp = stamp0 then Id (info, stamp', name)
	    else lookup (subst, id0)
	  | lookup (nil, id0) = id0

	fun substLongId (ShortId (info, id), subst) =
	    ShortId (info, lookup (subst, id))
	  | substLongId (longid as LongId (_, _, _), _) = longid

	fun substDecs' (decs, subst) =
	    Vector.map (fn dec => substDec' (dec, subst)) decs
	and substDec' (ValDec (info, pat, exp), subst) =
	    ValDec (info, substPat' (pat, subst), substExp' (exp, subst))
	  | substDec' (RecDec (info, decs), subst) =
	    RecDec (info, Vector.map (fn dec => substDec' (dec, subst)) decs)
	and substExp' (exp as LitExp (_, _), _) = exp
	  | substExp' (exp as PrimExp (_, _), _) = exp
	  | substExp' (exp as NewExp (_, _), _) = exp
	  | substExp' (VarExp (info, longid), subst) =
	    VarExp (info, substLongId (longid, subst))
	  | substExp' (TagExp (info, lab, exp, isNAry), subst) =
	    TagExp (info, lab, substExp' (exp, subst), isNAry)
	  | substExp' (ConExp (info, longid, exp, isNAry), subst) =
	    ConExp (info, longid, substExp' (exp, subst), isNAry)
	  | substExp' (exp as RefExp _, _) = exp
	  | substExp' (TupExp (info, exps), subst) =
	    TupExp (info, Vector.map (fn exp => substExp' (exp, subst)) exps)
	  | substExp' (ProdExp (info, expFields), subst) =
	    ProdExp (info,
		     Vector.map (fn Field (info, lab, exp) =>
				 Field (info, lab, substExp' (exp, subst)))
		     expFields)
	  | substExp' (SelExp (info, lab, exp), subst) =
	    SelExp (info, lab, substExp' (exp, subst))
	  | substExp' (VecExp (info, exps), subst) =
	    VecExp (info, Vector.map (fn exp => substExp' (exp, subst)) exps)
	  | substExp' (FunExp (info, matches), subst) =
	    FunExp (info, substMatches' (matches, subst))
	  | substExp' (AppExp (info, exp1, exp2), subst) =
	    AppExp (info, substExp' (exp1, subst), substExp' (exp2, subst))
	  | substExp' (AndExp (info, exp1, exp2), subst) =
	    AndExp (info, substExp' (exp1, subst), substExp' (exp2, subst))
	  | substExp' (OrExp (info, exp1, exp2), subst) =
	    OrExp (info, substExp' (exp1, subst), substExp' (exp2, subst))
	  | substExp' (IfExp (info, exp1, exp2, exp3), subst) =
	    IfExp (info, substExp' (exp1, subst),
		   substExp' (exp2, subst), substExp' (exp3, subst))
	  | substExp' (WhileExp (info, exp1, exp2), subst) =
	    WhileExp (info, substExp' (exp1, subst), substExp' (exp2, subst))
	  | substExp' (SeqExp (info, exps), subst) =
	    SeqExp (info, Vector.map (fn exp => substExp' (exp, subst)) exps)
	  | substExp' (CaseExp (info, exp, matches), subst) =
	    CaseExp (info, substExp' (exp, subst),
		     substMatches' (matches, subst))
	  | substExp' (RaiseExp (info, exp), subst) =
	    RaiseExp (info, substExp' (exp, subst))
	  | substExp' (HandleExp (info, exp, matches), subst) =
	    HandleExp (info, substExp' (exp, subst),
		       substMatches' (matches, subst))
	  | substExp' (exp as FailExp _, _) = exp
	  | substExp' (LazyExp (info, exp), subst) =
	    LazyExp (info, substExp' (exp, subst))
	  | substExp' (LetExp (info, decs, exp), subst) =
	    LetExp (info, substDecs' (decs, subst), substExp' (exp, subst))
	  | substExp' (UpExp (info, exp), subst) =
	    UpExp (info, substExp' (exp, subst))
	and substMatches' (matches, subst) =
	    Vector.map (fn Match (info, pat, exp) =>
			Match (info, substPat' (pat, subst),
			       substExp' (exp, subst))) matches
	and substPat' (pat as JokPat _, _) = pat
	  | substPat' (pat as LitPat (_, _), _) = pat
	  | substPat' (pat as VarPat (_, _), _) = pat
	  | substPat' (TagPat (info, lab, pat, isNAry), subst) =
	    TagPat (info, lab, substPat' (pat, subst), isNAry)
	  | substPat' (ConPat (info, longid, pat, isNAry), subst) =
	    ConPat (info, longid, substPat' (pat, subst), isNAry)
	  | substPat' (RefPat (info, pat), subst) =
	    RefPat (info, substPat' (pat, subst))
	  | substPat' (TupPat (info, pats), subst) =
	    TupPat (info, Vector.map (fn pat => substPat' (pat, subst)) pats)
	  | substPat' (ProdPat (info, patFields), subst) =
	    ProdPat (info,
		     Vector.map (fn Field (info, lab, pat) =>
				 Field (info, lab, substPat' (pat, subst)))
		     patFields)
	  | substPat' (VecPat (info, pats), subst) =
	    VecPat (info, Vector.map (fn pat => substPat' (pat, subst)) pats)
	  | substPat' (AsPat (info, pat1, pat2), subst) =
	    AsPat (info, substPat' (pat1, subst), substPat' (pat2, subst))
	  | substPat' (AltPat (info, pats), subst) =
	    AltPat (info, Vector.map (fn pat => substPat' (pat, subst)) pats)
	  | substPat' (NegPat (info, pat), subst) =
	    NegPat (info, substPat' (pat, subst))
	  | substPat' (GuardPat (info, pat, exp), subst) =
	    GuardPat (info, substPat' (pat, subst), substExp' (exp, subst))
	  | substPat' (WithPat (info, pat, decs), subst) =
	    WithPat (info, substPat' (pat, subst), substDecs' (decs, subst))

	fun substDecs (decs, subst as _::_) = substDecs' (decs, subst)
	  | substDecs (decs, nil) = decs
	fun substDec (dec, subst as _::_) = substDec' (dec, subst)
	  | substDec (dec, nil) = dec
	fun substExp (exp, subst as _::_) = substExp' (exp, subst)
	  | substExp (exp, nil) = exp

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
		    (fn (id, id', info) =>
		     ValDec (id_info info, VarPat (info, id),
			     VarExp (info, ShortId (info, id')))) subst
	    in
		case decs of
		    nil => pat'
		  | _::_ => WithPat (infoPat pat', pat', Vector.fromList decs)
	    end
	and relax (pat as JokPat _, subst) = (pat, subst)
	  | relax (pat as LitPat (_, _), subst) = (pat, subst)
	  | relax (VarPat (info, id), subst) =
	    let
		val id' = freshIntermediateId info
	    in
		(VarPat (info, id'), (id, id', info)::subst)
	    end
	  | relax (TagPat (info, lab, pat, isNAry), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
	    in
		(TagPat (info, lab, pat', isNAry), subst')
	    end
	  | relax (ConPat (info, longid, pat, isNAry), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
	    in
		(ConPat (info, longid, pat', isNAry), subst')
	    end
	  | relax (RefPat (info, pat), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
	    in
		(RefPat (info, pat'), subst')
	    end
	  | relax (TupPat (info, pats), subst) =
	    let
		val (pats', subst') =
		    Vector.foldr (fn (pat, (pats, subst)) =>
				  let
				      val (pat', subst') = relax (pat, subst)
				  in
				      (pat'::pats, subst')
				  end) (nil, subst) pats
	    in
		(TupPat (info, Vector.fromList pats'), subst')
	    end
	  | relax (ProdPat (info, patFields), subst) =
	    let
		val (patFields', subst') =
		    Vector.foldr
		    (fn (Field (info, lab, pat), (patFields, subst)) =>
		     let
			 val (pat', subst') = relax (pat, subst)
		     in
			 (Field (info, lab, pat')::patFields, subst')
		     end) (nil, subst) patFields
	    in
		(ProdPat (info, Vector.fromList patFields'), subst')
	    end
	  | relax (VecPat (info, pats), subst) =
	    let
		val (pats', subst') =
		    Vector.foldr (fn (pat, (pats, subst)) =>
				  let
				      val (pat', subst') = relax (pat, subst)
				  in
				      (pat'::pats, subst')
				  end) (nil, subst) pats
	    in
		(VecPat (info, Vector.fromList pats'), subst')
	    end
	  | relax (AsPat (info, pat1, pat2), subst) =
	    let
		val (pat1', subst') = relax (pat1, subst)
		val (pat2', subst'') = relax (pat2, subst')
	    in
		(AsPat (info, pat1', pat2'), subst'')
	    end
	  | relax (AltPat (info, pats), subst) =
	    (AltPat (info, Vector.map separateAlt pats), subst)
	  | relax (NegPat (info, pat), subst) =
	    (NegPat (info, separateAlt pat), subst)
	  | relax (GuardPat (info, pat, exp), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
		val subst'' =
		    List.map (fn (Id (_, stamp1, _), Id (_, stamp2, _), _) =>
			      (stamp1, stamp2)) subst'
	    in
		(GuardPat (info, pat', substExp (exp, subst'')), subst')
	    end
	  | relax (WithPat (info, pat, decs), subst) =
	    let
		val (pat', subst') = relax (pat, subst)
		val subst'' =
		    List.map (fn (Id (_, stamp1, _), Id (_, stamp2, _), _) =>
			      (stamp1, stamp2)) subst'
	    in
		(WithPat (info, pat', substDecs (decs, subst'')), subst')
	    end

	fun rowLabels row =
	    if Type.isEmptyRow row then
		if Type.isUnknownRow row then
		    raise Crash.Crash "IntermediateAux.rowLabels"
		else nil
	    else (#1 (Type.headRow row))::rowLabels (Type.tailRow row)

	fun isTuple (label::labels, i) =
	    if label = Label.fromInt i then isTuple (labels, i + 1)
	    else NONE
	  | isTuple (nil, i) = SOME (i - 1)

	fun rowToArity row =
	    let
		val labels = rowLabels row
	    in
		case isTuple (labels, 1) of
		    SOME n => Arity.Tuple n
		  | NONE => Arity.Product (Vector.fromList labels)
	    end

	fun typToArity typ =
	    if Type.isAll typ then
		typToArity (#2 (Type.asAll typ))
	    else if Type.isExist typ then
		typToArity (#2 (Type.asExist typ))
	    else if Type.isLambda typ then
		typToArity (#2 (Type.asLambda typ))
	    else if Type.isApply typ then
		typToArity (#1 (Type.asApply typ))
	    else if Type.isMu typ then
		typToArity (Type.asMu typ)
	    else if Type.isTuple typ then
		Arity.Tuple (Vector.length (Type.asTuple typ))
	    else if Type.isProd typ then
		rowToArity (Type.asProd typ)
	    else if Type.isSum typ then
		rowToArity (Type.asSum typ)
	    else Arity.Unary

	fun isZeroTyp typ =
	    Type.isCon typ andalso
	    Path.equals (#3 (Type.asCon typ), PervasiveType.path_zero)

	fun makeConArity (typ, isNAry) =
	    if isZeroTyp typ then NONE
	    else SOME (if isNAry then typToArity typ else Arity.Unary)

	fun find (labels, label', i, n) =
	    if i = n then NONE
	    else if Vector.sub (labels, i) = label' then
		SOME (O.Product labels, i)
	    else find (labels, label', i + 1, n)

	fun findLabel (Arity.Unary, label) =
	    raise Crash.Crash "IntermediateAux.findLabel"
	  | findLabel (Arity.Tuple n, label) =
	    (case Label.toLargeInt label of
		 SOME li =>
		     let
			 val i = LargeInt.toInt li
		     in
			 if i <= n then SOME (O.Tuple n, i - 1) else NONE
		     end
	       | NONE => NONE)
	  | findLabel (Arity.Product labels, label) =
	    find (labels, label, 0, Vector.length labels)

	fun labelToIndex (typ, label) =
	    case findLabel (typToArity typ, label) of
		SOME (prod, index) => (prod, index)
	      | NONE => raise Crash.Crash "IntermediateAux.labelToIndex"
    end
