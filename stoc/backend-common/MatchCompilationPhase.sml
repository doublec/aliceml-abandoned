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

structure Simplify :> SIMPLIFY =
    struct
	open IntermediateAux

	structure Intermediate = PostTranslationIntermediate
	structure Simplified = Simplified

	open PostTranslationIntermediate
	structure S = Simplified

	(* Pattern Matching Compilation: Tests *)

	datatype test =
	    LitTest of lit
	  | NameTest of longid
	  | ConTest of longid
	  | RefTest
	  | RecTest of arity
	  | LabTest of string
	  | GuardTest of mapping * exp
	  | DecTest of mapping * S.coord * dec list
	and arity =
	    TupArity of int
	  | RecArity of string list   (* sorted, no tuple *)
	withtype mapping = (string list * id) list

	(* Test Sequences *)

	type pos = string list

	datatype testSeqElem =
	    Test of pos * test
	  | Neg of testSeq
	  | Alt of testSeq list
	withtype testSeq = testSeqElem list

	(* Test Sequence Construction *)

	local
	    fun foldlind' (x::xr, f, z, i) =
		foldlind' (xr, f, f (i, x, z), i + 1)
	      | foldlind' (nil, _, z, _) = z
	in
	    fun foldlind f z xs = foldlind' (xs, f, z, 1)
	end

	local
	    structure StringLabelSort =
		LabelSort(type t = string fun get x = x)
	in
	    fun makeArity xs =
		case StringLabelSort.sort xs of
		    (_, StringLabelSort.Tup i) => TupArity i
		  | (xs', StringLabelSort.Rec) => RecArity xs'
	end

	fun makeTestSeq (WildPat _, _, rest, mapping) = (rest, mapping)
	  | makeTestSeq (LitPat (_, lit), pos, rest, mapping) =
	    (Test (pos, LitTest lit)::rest, mapping)
	  | makeTestSeq (VarPat (_, id), pos, rest, mapping) =
	    (rest, (pos, id)::mapping)
	  | makeTestSeq (ConPat (_, longid, patOpt), pos, rest, mapping) =
	    (case patOpt of
		 SOME pat => makeTestSeq (pat, ""::pos,
					  Test (pos, ConTest longid)::rest,
					  mapping)
	       | NONE => (Test (pos, NameTest longid)::rest, mapping))
	  | makeTestSeq (RefPat (_, pat), pos, rest, mapping) =
	    makeTestSeq (pat, ""::pos, Test (pos, RefTest)::rest, mapping)
	  | makeTestSeq (TupPat (_, pats), pos, rest, mapping) =
	    foldlind (fn (i, pat, (rest, mapping)) =>
		      makeTestSeq (pat, Int.toString i::pos, rest, mapping))
	    (Test (pos, RecTest (TupArity (List.length pats)))::rest, mapping)
	    pats
	  | makeTestSeq (RecPat (_, patFields, true), pos, rest, mapping) =
	    List.foldl (fn (Field (_, Lab (_, s), pat), (rest, mapping)) =>
			makeTestSeq (pat, s::pos, rest, mapping))
	    (List.foldl (fn (Field (_, Lab (_, s), _), rest) =>
			 Test (pos, LabTest s)::rest) rest patFields,
	     mapping) patFields
	  | makeTestSeq (RecPat (_, patFields, false), pos, rest, mapping) =
	    let
		val arity =
		    makeArity (List.map (fn Field (_, Lab (_, s), _) => s)
			       patFields)
	    in
		List.foldl (fn (Field (_, Lab (_, s), pat), (rest, mapping)) =>
			    makeTestSeq (pat, s::pos, rest, mapping))
		(Test (pos, RecTest arity)::rest, mapping) patFields
	    end
	  | makeTestSeq (AsPat (_, pat1, pat2), pos, rest, mapping) =
	    let
		val (rest', mapping') = makeTestSeq (pat1, pos, rest, mapping)
	    in
		makeTestSeq (pat2, pos, rest', mapping')
	    end
	  | makeTestSeq (AltPat (_, pats), pos, rest, mapping) =
	    (Alt (List.map (fn pat =>
			    let
				val (rest', _) =
				    makeTestSeq (pat, pos, nil, mapping)
			    in
				List.rev rest'
			    end) pats)::rest, mapping)
	  | makeTestSeq (NegPat (_, pat), pos, rest, mapping) =
	    let
		val (rest', _) = makeTestSeq (pat, pos, nil, mapping)
	    in
		(Neg (List.rev rest')::rest, mapping)
	    end
	  | makeTestSeq (GuardPat (_, pat, exp), pos, rest, mapping) =
	    makeTestSeq (pat, pos, Test (pos, GuardTest (mapping, exp))::rest,
			 mapping)
	  | makeTestSeq (WithPat (coord, pat, decs), pos, rest, mapping) =
	    let
		val (rest', mapping') = makeTestSeq (pat, pos, rest, mapping)
	    in
		(Test (pos, DecTest (mapping, coord, decs))::rest', mapping')
	    end

	(* Test Graphs *)

	datatype testGraph =
	    Node of pos * test * testGraph ref * testGraph ref *
		    testList ref * testList ref * int ref * S.exp option ref
	  | Leaf of S.exp * int ref * S.exp option ref
	  | Default
	withtype testList = (pos * test) list option

	(* Construction of Test Trees Needing Backtracking *)

	local
	    fun idEq (Id (_, stamp1, _), Id (_, stamp2, _)) = stamp1 = stamp2

	    fun longidEq (ShortId (_, id1), ShortId (_, id2)) = idEq (id1, id2)
	      | longidEq (LongId (_, longid1, id1), LongId (_, longid2, id2)) =
		longidEq (longid1, longid2) andalso idEq (id1, id2)
	      | longidEq (_, _) = false
	in
	    fun testEq (LitTest lit1, LitTest lit2) = lit1 = lit2
	      | testEq (NameTest longid1, NameTest longid2) =
		longidEq (longid1, longid2)
	      | testEq (ConTest longid1, ConTest longid2) =
		longidEq (longid1, longid2)
	      | testEq (RecTest arity1, RecTest arity2) = arity1 = arity2
	      | testEq (_, _) = false
	end

	fun areParallelTests (LitTest lit1, LitTest lit2) = lit1 <> lit2
	  | areParallelTests (LitTest _, RecTest _) = true
	  | areParallelTests (RecTest _, LitTest _) = true
	  | areParallelTests (RecTest arity1, RecTest arity2) =
	    arity1 <> arity2
	  | areParallelTests (_, _) = false

	local
	    fun findTest (tree, pos, test) =
		case tree of
		    Node (pos', test', thenTreeRef, ref elseTree,
			  _, _, count, _) =>
			if pos <> pos' then NONE
			else if testEq (test, test') then SOME thenTreeRef
			else if areParallelTests (test, test') then
			    findTest (elseTree, pos, test)
			else NONE
		  | _ => NONE
	in
	    fun mergeIntoTree (nil, thenTree, _) = thenTree
	      | mergeIntoTree (Test (pos, test)::testSeqRest,
			       thenTree, elseTree) =
		(case findTest (elseTree, pos, test) of
		     SOME treeRef =>
			 let
			     val newTree = mergeIntoTree (testSeqRest,
							  thenTree, !treeRef)
			 in
			     treeRef := newTree; elseTree
			 end
		   | NONE =>
			 let
			     val newThenTree =
				 mergeIntoTree (testSeqRest, thenTree, Default)
			 in
			     Node (pos, test, ref newThenTree, ref elseTree,
				   ref NONE, ref NONE, ref 0, ref NONE)
			 end)
	      | mergeIntoTree (Neg testSeq::testSeqRest, thenTree, elseTree) =
		mergeIntoTree (testSeq, elseTree,
			       mergeIntoTree (testSeqRest, thenTree, elseTree))
	      | mergeIntoTree (Alt testSeqs::testSeqRest, thenTree, elseTree) =
		let
		    val newThenTree =
			mergeIntoTree (testSeqRest, thenTree, elseTree)
		in
		    List.foldr (fn (testSeq, elseTree) =>
				mergeIntoTree (testSeq, newThenTree, elseTree))
		    elseTree testSeqs
		end
	end

	(* Elimination of Backtracking, Producing a Test Graph *)

	fun propagateElses (Node (_, _, thenTreeRef, elseTreeRef, _, _, _, _),
			    defaultTree) =
	    (case !elseTreeRef of
		 Default => elseTreeRef := defaultTree
	       | _ => propagateElses (!elseTreeRef, defaultTree);
	     case !thenTreeRef of
		 Default => thenTreeRef := defaultTree
	       | _ => propagateElses (!thenTreeRef, !elseTreeRef))
	  | propagateElses (Leaf (_, _, _), _) = ()
	  | propagateElses (Default, _) =
	    Crash.crash "Simplify.propagateElses"

	(*--** this can be optimized by imposing a total ordering on pos *)
	fun testSetMember (pos, test, (pos', test')::testSetRest) =
	    pos = pos' andalso testEq (test, test')
	    orelse testSetMember (pos, test, testSetRest)
	  | testSetMember (_, _, nil) = false

	local
	    fun testSetIntersect ((pos, test)::testSetRest, testSet') =
		if testSetMember (pos, test, testSet') then
		    (pos, test)::(testSetIntersect (testSetRest, testSet'))
		else testSetIntersect (testSetRest, testSet')
	      | testSetIntersect (nil, _) = nil
	in
	    fun computeTestSets (Node (pos, test, ref thenGraph, ref elseGraph,
				       trueTestsRef, falseTestsRef, _, _),
				  trueTests, falseTests) =
		(case !trueTestsRef of
		     NONE => trueTestsRef := SOME trueTests
		   | SOME trueTests' =>
			 trueTestsRef :=
			 SOME (testSetIntersect (trueTests, trueTests'));
		 case !falseTestsRef of
		     NONE => falseTestsRef := SOME falseTests
		   | SOME falseTests' =>
			 falseTestsRef :=
			 SOME (testSetIntersect (falseTests, falseTests'));
		 computeTestSets (thenGraph,
				  (pos, test)::trueTests, falseTests);
		 computeTestSets (elseGraph,
				  trueTests, (pos, test)::falseTests))
	      | computeTestSets (Leaf (_, _, _), _, _) = ()
	      | computeTestSets (Default, _, _) =
		Crash.crash "Simplify.computeTestSets"
	end

	fun disentailed (pos, test, (pos', test')::rest) =
	    pos = pos' andalso areParallelTests (test, test')
	    orelse disentailed (pos, test, rest)
	  | disentailed (_, _, nil) = false

	fun optimizeGraph (graphRef as
			   ref (Node (pos, test, thenGraphRef, elseGraphRef,
				      ref (SOME trueTests),
				      ref (SOME falseTests), count, _))) =
	    if testSetMember (pos, test, trueTests) then
		(graphRef := !thenGraphRef;
		 optimizeGraph graphRef)
	    else if testSetMember (pos, test, falseTests)
		orelse disentailed (pos, test, trueTests) then
		(graphRef := !elseGraphRef;
		 optimizeGraph graphRef)
	    else if !count = ~1 then ()
	    else
		(count := ~1;
		 optimizeGraph thenGraphRef;
		 optimizeGraph elseGraphRef)
	  | optimizeGraph (ref (Leaf (_, _, _))) = ()
	  | optimizeGraph _ = Crash.crash "Simplify.optimizeGraph"

	fun countShared (Node (_, _, ref thenGraph, ref elseGraph,
			       _, _, count, _)) =
	    let
		val n = !count
	    in
		count := (if n = ~1 then 1 else n + 1);
		if n < 1 then
		    (countShared thenGraph;
		     countShared elseGraph)
		else ()
	    end
	  | countShared (Leaf (_, count, _)) = count := !count + 1
	  | countShared Default = Crash.crash "Simplify.countShared"

	fun buildGraph (matches, elseExp) =
	    let
		val (graph, consequents) =
		    List.foldr (fn ((coord, pat, thenExp),
				    (elseTree, consequents)) =>
				let
				    val pat' = separateAlt pat
				    val (testSeq, _) =
					makeTestSeq (pat', nil, nil, nil)
				    val r = ref NONE
				    val leaf = Leaf (thenExp, ref 0, r)
				in
				    (mergeIntoTree (List.rev testSeq,
						    leaf, elseTree),
				     (coord, r)::consequents)
				end) (Default, nil) matches
		val r = ref 0
		val elseGraph = Leaf (elseExp, r, ref NONE)
	    in
		case graph of
		    Default => (r := 1; (elseGraph, consequents))
		  | _ => (propagateElses (graph, elseGraph);
			  computeTestSets (graph, nil, nil);
			  let
			      val graphRef = ref graph
			  in
			      optimizeGraph graphRef;
			      countShared (!graphRef);
			      (!graphRef, consequents)
			  end)
	    end

	(* Generate Simplified Ouput from Graph *)

	structure FieldLabelSort =
	    LabelSort(type t = lab * longid
		      fun get (Lab (_, s), _) = s)

	type mapping = (pos * id) list

	fun lookup (pos, (pos', id)::mappingRest) =
	    if pos = pos' then id
	    else lookup (pos, mappingRest)
	  | lookup (pos, nil) = Crash.crash "Simplify.lookup"

	fun mappingsToSubst (mapping0, mapping) =
	    List.map (fn (pos, id) => (lookup (pos, mapping), id)) mapping0

	fun makeRaise (coord, longid) =
	    S.RaiseExp (coord, S.VarExp (coord, longid))

	fun share exp =
	    (*--** replace Source.nowhere by S.info_exp exp *)
	    S.SharedExp (Source.nowhere, exp, ref Simplified.backendInfoDummy)

	fun makeShared (exp, ref 0) = Crash.crash "Simplify.makeShared"
	  | makeShared (exp, ref 1) = exp
	  | makeShared (exp, ref _) = share exp

	fun idToVarExp id =
	    let
		val coord = info_id id
	    in
		VarExp (coord, ShortId (coord, id))
	    end

(*
	fun derec (WildPat _, _) = nil
	  | derec (LitPat (_, _), _) =
	  | derec (VarPat (_, id), exp) = [(id, exp)]
	  | derec (ConPat (_, _, _), _) =
	  | derec (RefPat (_, _), _) =
	  | derec (TupPat (_, _), _) =
	  | derec (RecPat (_, _, _), _) =
	  | derec (AsPat (_, pat1, pat2), exp) =
	    derec (pat1, exp) @ derec (pat2, exp)
	  | derec (AltPat (_, _), _) =
	  | derec (NegPat (_, _), _) =
	  | derec (GuardPat (_, _, _), _) =
	  | derec (WithPat (_, _, _), _) =
*)

	fun simplifyDec (ValDec (coord, VarPat (_, id), exp, false)) =
	    (* this is needed to end recursion with introduced WithPats *)
	    S.OneDec (coord, id, simplifyExp exp)
	  | simplifyDec (ValDec (coord, VarPat (_, id), exp, true)) =
	    S.RecDec (coord, [(id, simplifyExp exp)])
	  | simplifyDec (ValDec (coord, pat, exp, false)) =
	    let
		val ids = patternVariablesOf pat
		val decExp = S.DecExp (coord, ids)
		val matches = [(coord, pat, decExp)]
	    in
		S.ValDec (coord, ids,
			  simplifyCase (coord, exp, matches, longid_Bind))
	    end
	  | simplifyDec (ValDec (coord, pat, exp, true)) =
	    Crash.crash "Simplify.simplifyDec: not implemented"   (*--** *)
	  | simplifyDec (ConDec (coord, id, hasArgs)) =
	    S.ConDec (coord, id, hasArgs)
	and simplifyTerm (VarExp (_, longid)) = (NONE, longid)
	  | simplifyTerm exp =
	    let
		val coord = info_exp exp
		val id' = freshId coord
		val dec' = S.OneDec (coord, id', simplifyExp exp)
	    in
		(SOME dec', ShortId (coord, id'))
	    end
	and simplifyExp (LitExp (coord, lit)) =
	    S.LitExp (coord, lit)
	  | simplifyExp (VarExp (coord, longid)) =
	    S.VarExp (coord, longid)
	  | simplifyExp (ConExp (coord, longid, SOME exp)) =
	    let
		val (decOpt, longid) = simplifyTerm exp
		val exp' = S.ConExp (coord, longid, SOME longid)
	    in
		case decOpt of
		    NONE => exp'
		  | SOME dec' => S.LetExp (info_exp exp, [dec'], exp')
	    end
	  | simplifyExp (ConExp (coord, longid, NONE)) =
	    S.ConExp (coord, longid, NONE)
	  | simplifyExp (RefExp (coord, SOME exp)) =
	    let
		val (decOpt, longid) = simplifyTerm exp
		val exp' = S.ConExp (coord, longid_ref, SOME longid)
	    in
		case decOpt of
		    NONE => exp'
		  | SOME dec' => S.LetExp (info_exp exp, [dec'], exp')
	    end
	  | simplifyExp (RefExp (coord, NONE)) =
	    S.ConExp (coord, longid_ref, NONE)
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
		val exp' = S.TupExp (coord, longids)
	    in
		case decs' of
		    nil => exp'
		  | _::_ => S.LetExp (coord, decs', exp')
	    end
	  | simplifyExp (RecExp (coord, expFields)) =
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
			    S.TupExp (coord,
				      List.map (fn (_, exp') => exp') fields')
		      | (fields', FieldLabelSort.Rec) =>
			    S.RecExp (coord, fields)
	    in
		case decs' of
		    nil => exp'
		  | _::_ => S.LetExp (coord, decs', exp')
	    end
	  | simplifyExp (SelExp (coord, lab)) =
	    S.SelExp (coord, lab)
	  | simplifyExp (FunExp (coord, id, exp)) =
	    (*--** name propagation *)
	    S.FunExp (coord, "", id, simplifyExp exp)
	  | simplifyExp (AppExp (coord, exp1, exp2)) =
	    S.AppExp (coord, simplifyExp exp1, simplifyExp exp2)
	  | simplifyExp (AdjExp (coord, exp1, exp2)) =
	    S.AdjExp (coord, simplifyExp exp1, simplifyExp exp2)
	  | simplifyExp (AndExp (coord, exp1, exp2)) =
	    simplifyExp (IfExp (coord, exp1,
				exp2, VarExp (coord, longid_false)))
	  | simplifyExp (OrExp (coord, exp1, exp2)) =
	    simplifyExp (IfExp (coord, exp1,
				VarExp (coord, longid_true), exp2))
	  | simplifyExp (IfExp (_, exp1, exp2, exp3)) =
	    simplifyIf (exp1, simplifyExp exp2, simplifyExp exp3)
	  | simplifyExp (WhileExp (coord, exp1, exp2)) =
	    S.WhileExp (coord, simplifyExp exp1, simplifyExp exp2)
	  | simplifyExp (SeqExp (coord, exps)) =
	    S.SeqExp (coord, List.map simplifyExp exps)
	  | simplifyExp (CaseExp (coord, exp, matches)) =
	    let
		val matches' =
		    List.map (fn Match (_, pat, exp) =>
			      (info_exp exp, pat, simplifyExp exp)) matches
	    in
		simplifyCase (coord, exp, matches', longid_Match)
	    end
	  | simplifyExp (RaiseExp (coord, exp)) =
	    S.RaiseExp (coord, simplifyExp exp)
	  | simplifyExp (HandleExp (coord, exp, matches)) =
	    let
		val id = freshId coord
		val longid = ShortId (coord, id)
		val raiseExp = RaiseExp (coord, VarExp (coord, longid))
		val reraise = Match (coord, WildPat coord, raiseExp)
		val exp' = CaseExp (coord, VarExp (coord, longid),
				    matches @ [reraise])
	    in
		S.HandleExp (coord, simplifyExp exp, id, simplifyExp exp')
	    end
	  | simplifyExp (LetExp (coord, decs, exp)) =
	    S.LetExp (coord, List.map simplifyDec decs, simplifyExp exp)
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
		val coord = info_exp exp
		val (decOpt, longid) = simplifyTerm exp
		val errExp = makeRaise (coord, longid_Match)
		val exp' = S.TestExp (coord, longid,
				      S.NameTest longid_true, thenExp,
				      S.TestExp (coord, longid,
						 S.NameTest longid_false,
						 elseExp, errExp))
	    in
		case decOpt of
		    NONE => exp'
		  | SOME dec' => S.LetExp (coord, [dec'], exp')
	    end
	and simplifyCase (coord, exp, matches, longid) =
	    let
		val (decOpt, id) =
		    case exp of
			VarExp (_, ShortId (_, id')) => (NONE, id')
		      | _ => let
				 val coord = info_exp exp
				 val id' = freshId coord
				 val dec' =
				     S.OneDec (coord, id', simplifyExp exp)
			     in
				 (SOME dec', id')
			     end
		val errExp = S.RaiseExp (coord, S.VarExp (coord, longid))
		val (graph, consequents) = buildGraph (matches, errExp)
		val exp' = simplifyGraph (graph, [(nil, id)])
	    in
		List.app (fn (coord, ref expOpt) =>
			  case expOpt of
			      NONE => Error.error (coord, "unreachable")
			    | SOME _ => ()) consequents;
		case decOpt of
		    NONE => exp'
		  | SOME dec' => S.LetExp (info_exp exp, [dec'], exp')
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
	    Crash.crash "Simplify.simplifyGraph"
	and simplifyNode (pos, GuardTest (mapping0, exp),
			  thenGraph, elseGraph, mapping) =
	    let
		val coord = Source.nowhere
		val id = freshId coord
		val longid = ShortId (coord, id)
		val subst = mappingsToSubst (mapping0, mapping)
		val dec' =
		    S.OneDec (coord, id, simplifyExp (substExp (exp, subst)))
		val thenExp = simplifyGraph (thenGraph, mapping)
		val elseExp = simplifyGraph (elseGraph, mapping)
		val errExp = makeRaise (coord, longid_Match)
	    in
		S.LetExp (coord, [dec'],
			  S.TestExp (coord, longid,
				     S.NameTest longid_true, thenExp,
				     S.TestExp (coord, longid,
						S.NameTest longid_false,
						elseExp, errExp)))
	    end
	  | simplifyNode (pos, DecTest (mapping0, coord, decs),
			  thenGraph, _, mapping) =
	    let
		val subst = mappingsToSubst (mapping0, mapping)
		val decs' =
		    List.map (fn dec => simplifyDec (substDec (dec, subst)))
		    decs
		val thenExp = simplifyGraph (thenGraph, mapping)
	    in
		S.LetExp (coord, decs', thenExp)
	    end
	  | simplifyNode (pos, test, thenGraph, elseGraph, mapping) =
	    let
		val longid = ShortId (Source.nowhere, lookup (pos, mapping))
		val (test', mapping') = simplifyTest (test, pos, mapping)
		val thenExp = simplifyGraph (thenGraph, mapping')
		val elseExp = simplifyGraph (elseGraph, mapping')
	    in
		S.TestExp (Source.nowhere, longid, test', thenExp, elseExp)
	    end
	and simplifyTest (LitTest lit, _, mapping) =
	    (S.LitTest lit, mapping)
	  | simplifyTest (NameTest longid, _, mapping) =
	    (S.NameTest longid, mapping)
	  | simplifyTest (ConTest longid, pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((""::pos), id)::mapping
	    in
		(S.ConTest (longid, id), mapping')
	    end
	  | simplifyTest (RefTest, pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((""::pos), id)::mapping
	    in
		(S.ConTest (longid_ref, id), mapping')
	    end
	  | simplifyTest (RecTest arity, pos, mapping) =
	    let
		val labels =
		    case arity of
			TupArity i =>
			    List.tabulate (i, fn i => Int.toString (i + 1))
		      | RecArity ss => ss
		val stringIdList =
		    List.map (fn s => (s, freshId Source.nowhere)) labels
		val mapping' =
		    ListPair.foldr (fn (s, (_, i), mapping) =>
				    (s::pos, i)::mapping)
		    mapping (labels, stringIdList)
	    in
		(S.RecTest stringIdList, mapping')
	    end
	  | simplifyTest (LabTest string, pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((string::pos), id)::mapping
	    in
		(S.LabTest (string, id), mapping')
	    end
	  | simplifyTest ((GuardTest (_, _) | DecTest (_, _, _)), _, _) =
	    Crash.crash "Simplify.simplifyTest"
    end
