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

	fun makeShared (exp, ref 0) =
	    Crash.crash "MatchCompilationPhase.makeShared"
	  | makeShared (exp, ref 1) = exp
	  | makeShared (exp, ref _) = share exp

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
		    List.map (fn (id, exp) =>   (*--** simplifyExp is wrong! *)
			      (id, simplifyExp (substExp (exp, subst))))
		    idExpList
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
	and simplifyExp (LitExp (coord, lit)) = O.LitExp (coord, lit)
	  | simplifyExp (VarExp (coord, longid)) = O.VarExp (coord, longid)
	  | simplifyExp (ConExp (coord, longid, hasArgs)) =
	    O.ConExp (coord, longid, NONE, hasArgs)
	  | simplifyExp (RefExp coord) =
	    O.ConExp (coord, longid_ref, NONE, true)
	  | simplifyExp (TupExp (coord, exps)) =
	    let
		val (decs', longids) =
		    List.foldr
		    (fn (exp, (decs', longids)) =>
		     let
			 val (decOpt, longid) = simplifyTerm exp
		     in
			 case decOpt of
			     NONE => (decs', longid::longids)
			   | SOME dec' => (dec'::decs', longid::longids)
		     end) (nil, nil) exps
		val exp' = O.TupExp (coord, longids)
	    in
		case decs' of
		    nil => exp'
		  | _::_ => O.LetExp (coord, decs', exp')
	    end
	  | simplifyExp (RowExp (coord, expFields)) =
	    let
		val (decs', fields) =
		    List.foldr
		    (fn (Field (_, lab, exp), (decs', fields)) =>
		     let
			 val (decOpt, longid) = simplifyTerm exp
			 val field = (lab, longid)
		     in
			 case decOpt of
			     NONE => (decs', field::fields)
			   | SOME dec' => (dec'::decs', field::fields)
		     end) (nil, nil) expFields
		val exp' =
		    case FieldLabelSort.sort fields of
			(fields', FieldLabelSort.Tup _) =>
			    O.TupExp (coord,
				      List.map (fn (_, exp') => exp') fields')
		      | (fields', FieldLabelSort.Rec) =>
			    O.RecExp (coord, fields)
	    in
		case decs' of
		    nil => exp'
		  | _::_ => O.LetExp (coord, decs', exp')
	    end
	  | simplifyExp (SelExp (coord, lab)) = O.SelExp (coord, lab, NONE)
	  | simplifyExp (VecExp (coord, exps)) =
	    let
		val (decs', longids) =
		    List.foldr
		    (fn (exp, (decs', longids)) =>
		     let
			 val (decOpt, longid) = simplifyTerm exp
		     in
			 case decOpt of
			     NONE => (decs', longid::longids)
			   | SOME dec' => (dec'::decs', longid::longids)
		     end) (nil, nil) exps
		val exp' = O.VecExp (coord, longids)
	    in
		case decs' of
		    nil => exp'
		  | _::_ => O.LetExp (coord, decs', exp')
	    end
	  | simplifyExp (FunExp (coord, id, exp)) =
	    (*--** name propagation, multiple argument optimization *)
	    O.FunExp (coord, "", [(O.OneArg id, simplifyExp exp)])
	  | simplifyExp (AppExp (coord, ConExp (_, longid, true), exp)) =
	    let
		val (decOpt, longid') = simplifyTerm exp
		val exp' = O.ConExp (coord, longid, SOME longid', true)
	    in
		case decOpt of
		    NONE => exp'
		  | SOME dec' => O.LetExp (coord, [dec'], exp')
	    end
	  | simplifyExp (AppExp (coord, RefExp _, exp)) =
	    let
		val (decOpt, longid) = simplifyTerm exp
		val exp' = O.ConExp (coord, longid_ref, SOME longid, true)
	    in
		case decOpt of
		    NONE => exp'
		  | SOME dec' => O.LetExp (coord, [dec'], exp')
	    end
	  | simplifyExp (AppExp (coord, exp1, exp2)) =
	    let
		val (decOpt, longid) = simplifyTerm exp1
		val exp' =
		    O.AppExp (coord, longid, simplifyExp exp2, ref false)
	    in
		case decOpt of
		    NONE => exp'
		  | SOME dec' => O.LetExp (coord, [dec'], exp')
	    end
	  | simplifyExp (AdjExp (coord, exp1, exp2)) =
	    O.AdjExp (coord, simplifyExp exp1, simplifyExp exp2)
	  | simplifyExp (AndExp (coord, exp1, exp2)) =
	    simplifyExp (IfExp (coord, exp1,
				exp2, VarExp (coord, longid_false)))
	  | simplifyExp (OrExp (coord, exp1, exp2)) =
	    simplifyExp (IfExp (coord, exp1,
				VarExp (coord, longid_true), exp2))
	  | simplifyExp (IfExp (_, exp1, exp2, exp3)) =
	    simplifyIf (exp1, simplifyExp exp2, simplifyExp exp3)
	  | simplifyExp (WhileExp (coord, exp1, exp2)) =
	    O.WhileExp (coord, simplifyExp exp1, simplifyExp exp2)
	  | simplifyExp (SeqExp (coord, exps)) =
	    O.SeqExp (coord, List.map simplifyExp exps)
	  | simplifyExp (CaseExp (coord, exp, matches)) =
	    let
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (infoExp exp, pat, simplifyExp exp)) matches
	    in
		simplifyCase (coord, exp, matches', longid_Match)
	    end
	  | simplifyExp (RaiseExp (coord, exp)) =
	    O.RaiseExp (coord, simplifyExp exp)
	  | simplifyExp (HandleExp (coord, exp, matches)) =
	    let
		val id = freshId coord
		val longid = ShortId (coord, id)
		val varExp = VarExp (coord, longid)
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (infoExp exp, pat, simplifyExp exp)) matches
	    in
		O.HandleExp (coord, simplifyExp exp, id,
			     simplifyCase (coord, varExp, matches', longid))
	    end
	  | simplifyExp (LetExp (coord, decs, exp)) =
	    O.LetExp (coord, simplifyDecs decs, simplifyExp exp)
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
	and simplifyGraph (Node (_, _, _, _, _, _, _, ref (SOME exp)), _) = exp
	  | simplifyGraph (Node (pos, test, ref thenGraph, ref elseGraph, _, _,
				 count, expOptRef as ref NONE), mapping) =
	    let
		val exp = makeShared (simplifyNode (pos, test, thenGraph,
						    elseGraph, mapping), count)
	    in
		expOptRef := SOME exp;
		exp
	    end
	  | simplifyGraph (Leaf (_, _, ref (SOME exp)), _) = exp
	  | simplifyGraph (Leaf (exp, count, expOptRef as ref NONE), _) =
	    let
		val exp' = makeShared (exp, count)
	    in
		expOptRef := SOME exp';
		exp'
	    end
	  | simplifyGraph (Default, _) =
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
