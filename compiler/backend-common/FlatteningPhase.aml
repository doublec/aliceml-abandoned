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
	structure O = SimplifiedGrammar

	open I
	open IntermediateAux
	open SimplifyMatch
	open Prebound

	val id_false = Id(Source.nowhere, stamp_false, ExId "false")
	val id_true = Id(Source.nowhere, stamp_true,  ExId "true")
	val id_ref = Id(Source.nowhere, stamp_ref,   ExId "ref")
	val id_Match = Id(Source.nowhere, stamp_Match, ExId "Match")
	val id_Bind = Id(Source.nowhere, stamp_Bind,  ExId "Bind")

	val longid_false = ShortId(Source.nowhere, id_false)
	val longid_true = ShortId(Source.nowhere, id_true)
	val longid_ref = ShortId(Source.nowhere, id_ref)
	val longid_Match = ShortId(Source.nowhere, id_Match)
	val longid_Bind = ShortId(Source.nowhere, id_Bind)

	structure FieldLabelSort =
	    MakeLabelSort(type 'a t = lab * longid
			  fun get (Lab (_, s), _) = s)

	type mapping = (pos * id) list

	fun lookup (pos, (pos', id)::mappingRest) =
	    if pos = pos' then id
	    else lookup (pos, mappingRest)
	  | lookup (pos, nil) = Crash.crash "MatchCompilationPhase.lookup"

	fun mappingsToSubst (mapping0, mapping) =
	    List.map (fn (pos, id) => (id, lookup (pos, mapping))) mapping0

	fun makeRaise (coord, longid) =
	    O.RaiseExp (coord, O.VarExp (coord, longid))

	fun share exp =
	    O.SharedExp (O.coordOf exp, exp, ref O.backendInfoDummy)

	fun idToVarExp id =
	    let
		val coord = infoId id
	    in
		VarExp (coord, ShortId (coord, id))
	    end

	fun simplifyDecs (ValDec (coord, VarPat (_, id), exp)::decr) =
	    (* this is needed to end recursion with introduced WithPats *)
	    O.OneDec (coord, id, simplifyExp exp)::simplifyDecs decr
	  | simplifyDecs (ValDec (coord, pat, exp)::decr) =
	    let
		val ids = patternVariablesOf pat
		val decExp = O.DecExp (coord, ids)
		val matches = [(coord, pat, decExp)]
	    in
		O.ValDec (coord, ids,
			  simplifyCase (coord, exp, matches, longid_Bind))::
		simplifyDecs decr
	    end
	  | simplifyDecs (RecDec (coord, decs)::decr) =
	    let
		val (conDecs, constraints, idExpList, subst) =
		    SimplifyRec.derec decs
		val aliasDecs =
		    List.map (fn (fromId, toId) =>
			      let
				  val coord = infoId toId
				  val toExp =
				      O.VarExp (coord, ShortId (coord, toId))
			      in
				  O.OneDec (infoId fromId, fromId, toExp)
			      end) subst
		val idExpList' =
		    List.foldr (fn ((id, exp), bindings) =>
				let
				    val (bindings', exp') =
					unfoldExp (substExp (exp, subst))
				in
				    (id, exp')::(bindings' @ bindings)
				end) nil idExpList
	    in
		simplifyDecs conDecs @
		(*--** generate constraints *)
		O.RecDec (coord, idExpList')::(aliasDecs @ simplifyDecs decr)
	    end
	  | simplifyDecs (ConDec (coord, id, hasArgs)::decr) =
	    O.ConDec (coord, id, hasArgs)::simplifyDecs decr
	  | simplifyDecs nil = nil
	and simplifyTerm (VarExp (_, longid)) = (NONE, longid)
	  | simplifyTerm exp =
	    let
		val coord = infoExp exp
		val id' = freshId coord
		val dec' = O.OneDec (coord, id', simplifyExp exp)
	    in
		(SOME dec', ShortId (coord, id'))
	    end
	and simplifyExp exp =
	    case unfoldExp exp of
		(nil, exp') => exp'
	      | (bindings as _::_, exp') =>
		    O.LetExp (infoExp exp,
			      List.map (fn (id, exp) =>
					O.OneDec (infoId id, id, exp))
			      bindings, exp')
	and unfoldTerm (VarExp (_, longid)) = (nil, longid)
	  | unfoldTerm exp =
	    let
		val coord = infoExp exp
		val id' = freshId coord
		val (bindings, exp') = unfoldExp exp
	    in
		(bindings @ [(id', exp')], ShortId (coord, id'))
	    end
	and unfoldExp (LitExp (coord, lit)) = (nil, O.LitExp (coord, lit))
	  | unfoldExp (VarExp (coord, longid)) =
	    (nil, O.VarExp (coord, longid))
	  | unfoldExp (ConExp (coord, longid, hasArgs)) =
	    (nil, O.ConExp (coord, longid, NONE, hasArgs))
	  | unfoldExp (RefExp coord) =
	    (nil, O.ConExp (coord, longid_ref, NONE, true))
	  | unfoldExp (TupExp (coord, exps)) =
	    let
		val (bindings, longids) =
		    List.foldr
		    (fn (exp, (bindings, longids)) =>
		     let
			 val (bindings', longid) = unfoldTerm exp
		     in
			 (bindings' @ bindings, longid::longids)
		     end) (nil, nil) exps
	    in
		(bindings, O.TupExp (coord, longids))
	    end
	  | unfoldExp (RowExp (coord, expFields)) =
	    let
		val (bindings, fields) =
		    List.foldr
		    (fn (Field (_, lab, exp), (bindings, fields)) =>
		     let
			 val (bindings', longid) = unfoldTerm exp
		     in
			 (bindings' @ bindings, (lab, longid)::fields)
		     end) (nil, nil) expFields
		val exp' =
		    case FieldLabelSort.sort fields of
			(fields', FieldLabelSort.Tup _) =>
			    O.TupExp (coord, List.map #2 fields')
		      | (fields', FieldLabelSort.Rec) =>
			    O.RecExp (coord, fields')
	    in
		(bindings, exp')
	    end
	  | unfoldExp (SelExp (coord, lab)) =
	    (nil, O.SelExp (coord, lab, NONE))
	  | unfoldExp (VecExp (coord, exps)) =
	    let
		val (bindings, longids) =
		    List.foldr
		    (fn (exp, (bindings, longids)) =>
		     let
			 val (bindings', longid) = unfoldTerm exp
		     in
			 (bindings' @ bindings, longid::longids)
		     end) (nil, nil) exps
	    in
		(bindings, O.VecExp (coord, longids))
	    end
	  | unfoldExp (FunExp (coord, id, exp)) =
	    (*--** name propagation, multiple argument optimization *)
	    (nil, O.FunExp (coord, "", [(O.OneArg id, simplifyExp exp)]))
	  | unfoldExp (AppExp (coord, ConExp (_, longid, true), exp)) =
	    let
		val (bindings, longid') = unfoldTerm exp
	    in
		(bindings, O.ConExp (coord, longid, SOME longid', true))
	    end
	  | unfoldExp (AppExp (coord, RefExp _, exp)) =
	    let
		val (bindings, longid) = unfoldTerm exp
	    in
		(bindings, O.ConExp (coord, longid_ref, SOME longid, true))
	    end
	  | unfoldExp (AppExp (coord, exp1, exp2)) =
	    let
		val (bindings, longid) = unfoldTerm exp1
	    in
		(bindings,
		 O.AppExp (coord, longid, simplifyExp exp2, ref false))
	    end
	  | unfoldExp (AdjExp (coord, exp1, exp2)) =
	    let
		val (bindings1, longid1) = unfoldTerm exp1
		val (bindings2, longid2) = unfoldTerm exp2
	    in
		(bindings1 @ bindings2, O.AdjExp (coord, longid1, longid2))
	    end
	  | unfoldExp (AndExp (coord, exp1, exp2)) =
	    unfoldExp (IfExp (coord, exp1,
			      exp2, VarExp (coord, longid_false)))
	  | unfoldExp (OrExp (coord, exp1, exp2)) =
	    unfoldExp (IfExp (coord, exp1,
			      VarExp (coord, longid_true), exp2))
	  | unfoldExp (IfExp (_, exp1, exp2, exp3)) =
	    (nil, simplifyIf (exp1, simplifyExp exp2, simplifyExp exp3))
	  | unfoldExp (WhileExp (coord, exp1, exp2)) =
	    (nil, O.WhileExp (coord, simplifyExp exp1, simplifyExp exp2))
	  | unfoldExp (SeqExp (coord, exps)) =
	    (nil, O.SeqExp (coord, List.map simplifyExp exps))
	  | unfoldExp (CaseExp (coord, exp, matches)) =
	    let
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (infoExp exp, pat, simplifyExp exp)) matches
	    in
		(nil, simplifyCase (coord, exp, matches', longid_Match))
	    end
	  | unfoldExp (RaiseExp (coord, exp)) =
	    (nil, O.RaiseExp (coord, simplifyExp exp))
	  | unfoldExp (HandleExp (coord, exp, matches)) =
	    let
		val id = freshId coord
		val longid = ShortId (coord, id)
		val varExp = VarExp (coord, longid)
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (infoExp exp, pat, simplifyExp exp)) matches
		val exp' = simplifyCase (coord, varExp, matches', longid)
	    in
		(nil, O.HandleExp (coord, simplifyExp exp, id, exp'))
	    end
	  | unfoldExp (LetExp (coord, decs, exp)) =
	    (nil, O.LetExp (coord, simplifyDecs decs, simplifyExp exp))
	and simplifyIf (AndExp (_, exp1, exp2), thenExp, elseExp) =
	    let
		val elseExp' = share elseExp
		val thenExp' = simplifyIf (exp2, thenExp, elseExp')
	    in
		simplifyIf (exp1, thenExp', elseExp')
	    end
	  | simplifyIf (OrExp (_, exp1, exp2), thenExp, elseExp) =
	    let
		val thenExp' = share thenExp
		val elseExp' = simplifyIf (exp2, thenExp', elseExp)
	    in
		simplifyIf (exp1, thenExp', elseExp')
	    end
	  | simplifyIf (exp, thenExp, elseExp) =
	    let
		val coord = infoExp exp
		val (decOpt, longid) = simplifyTerm exp
		val errExp = makeRaise (coord, longid_Match)
		val exp' =
		    O.TestExp (coord, longid,
			       O.ConTest (longid_true, NONE), thenExp,
			       O.TestExp (coord, longid,
					  O.ConTest (longid_false, NONE),
					  elseExp, errExp))
	    in
		case decOpt of
		    NONE => exp'
		  | SOME dec' => O.LetExp (coord, [dec'], exp')
	    end
	and simplifyCase (coord, exp, matches, longid) =
	    let
		val (decOpt, id) =
		    case exp of
			VarExp (_, ShortId (_, id')) => (NONE, id')
		      | _ => let
				 val coord = infoExp exp
				 val id' = freshId coord
				 val dec' =
				     O.OneDec (coord, id', simplifyExp exp)
			     in
				 (SOME dec', id')
			     end
		val errExp = O.RaiseExp (coord, O.VarExp (coord, longid))
		val (graph, consequents) = buildGraph (matches, errExp)
		val exp' = simplifyGraph (graph, [(nil, id)])
	    in
		List.app (fn (coord, ref expOpt) =>
			  case expOpt of
			      NONE =>
				  Error.error (coord, "unreachable expression")
			    | SOME _ => ()) consequents;
		case decOpt of
		    NONE => exp'
		  | SOME dec' => O.LetExp (coord, [dec'], exp')
	    end
	and simplifyGraph (Node (pos, test, ref thenGraph, ref elseGraph,
				 status as ref (Optimized (_, _))), mapping) =
	    let
		val exp =
		    share (simplifyNode (pos, test, thenGraph, elseGraph,
					 mapping))
	    in
		status := Simplified exp; exp
	    end
	  | simplifyGraph (Node (_, _, _, _, ref (Simplified exp)), _) = exp
	  | simplifyGraph (Leaf (exp, expOptRef as ref NONE), _) =
	    let
		val exp' = share exp
	    in
		expOptRef := SOME exp'; exp'
	    end
	  | simplifyGraph (Leaf (_, ref (SOME exp)), _) = exp
	  | simplifyGraph (_, _) =
	    Crash.crash "MatchCompilationPhase.simplifyGraph"
	and simplifyNode (pos, GuardTest (mapping0, exp),
			  thenGraph, elseGraph, mapping) =
	    let
		val coord = Source.nowhere
		val id = freshId coord
		val longid = ShortId (coord, id)
		val subst = mappingsToSubst (mapping0, mapping)
		val dec' =
		    O.OneDec (coord, id, simplifyExp (substExp (exp, subst)))
		val thenExp = simplifyGraph (thenGraph, mapping)
		val elseExp = simplifyGraph (elseGraph, mapping)
		val errExp = makeRaise (coord, longid_Match)
	    in
		O.LetExp (coord, [dec'],
			  O.TestExp (coord, longid,
				     O.ConTest (longid_true, NONE), thenExp,
				     O.TestExp (coord, longid,
						O.ConTest (longid_false, NONE),
						elseExp, errExp)))
	    end
	  | simplifyNode (pos, DecTest (mapping0, coord, decs),
			  thenGraph, _, mapping) =
	    let
		val subst = mappingsToSubst (mapping0, mapping)
		val decs' =
		    simplifyDecs
		    (List.map (fn dec => substDec (dec, subst)) decs)
		val thenExp = simplifyGraph (thenGraph, mapping)
	    in
		O.LetExp (coord, decs', thenExp)
	    end
	  | simplifyNode (pos, test, thenGraph, elseGraph, mapping) =
	    let
		val longid = ShortId (Source.nowhere, lookup (pos, mapping))
		val (test', mapping') = simplifyTest (test, pos, mapping)
		val thenExp = simplifyGraph (thenGraph, mapping')
		val elseExp = simplifyGraph (elseGraph, mapping')
	    in
		O.TestExp (Source.nowhere, longid, test', thenExp, elseExp)
	    end
	and simplifyTest (LitTest lit, _, mapping) =
	    (O.LitTest lit, mapping)
	  | simplifyTest (ConTest (longid, false), _, mapping) =
	    (O.ConTest (longid, NONE), mapping)
	  | simplifyTest (ConTest (longid, true), pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((""::pos), id)::mapping
	    in
		(O.ConTest (longid, SOME id), mapping')
	    end
	  | simplifyTest (RefTest, pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((""::pos), id)::mapping
	    in
		(O.ConTest (longid_ref, SOME id), mapping')
	    end
	  | simplifyTest (TupTest n, pos, mapping) =
	    let
		val ids = List.tabulate (n, fn _ => freshId Source.nowhere)
		val labs = List.tabulate (n, fn i => Int.toString (i + 1))
		val mapping' =
		    foldli (fn (i, id, mapping) =>
			    (Int.toString i::pos, id)::mapping) mapping ids
	    in
		(O.TupTest ids, mapping')
	    end
	  | simplifyTest (RecTest labs, pos, mapping) =
	    let
		val stringIdList =
		    List.map (fn s => (s, freshId Source.nowhere)) labs
		val mapping' =
		    ListPair.foldr (fn (s, (_, i), mapping) =>
				    (s::pos, i)::mapping)
		    mapping (labs, stringIdList)
	    in
		(O.RecTest stringIdList, mapping')
	    end
	  | simplifyTest (LabTest string, pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((string::pos), id)::mapping
	    in
		(O.LabTest (string, id), mapping')
	    end
	  | simplifyTest (VecTest n, pos, mapping) =
	    let
		val ids = List.tabulate (n, fn _ => freshId Source.nowhere)
		val labs = List.tabulate (n, fn i => Int.toString (i + 1))
		val mapping' =
		    foldli (fn (i, id, mapping) =>
			    (Int.toString i::pos, id)::mapping) mapping ids
	    in
		(O.VecTest ids, mapping')
	    end
	  | simplifyTest ((GuardTest (_, _) | DecTest (_, _, _)), _, _) =
	    Crash.crash "MatchCompilationPhase.simplifyTest"

	fun simplify (decs, ids) = (simplifyDecs decs, ids)
    end
