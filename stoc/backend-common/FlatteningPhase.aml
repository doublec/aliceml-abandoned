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
	open Prebound

	(* Pattern Matching Compilation: Tests *)

	datatype test =
	    LitTest of lit
	  | ConTest of longid * bool   (* has args *)
	  | RefTest
	  | TupTest of int
	  | RecTest of string list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of string
	  | GuardTest of mapping * exp
	  | DecTest of mapping * O.coord * dec list
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

	structure StringLabelSort =
	    LabelSort(type t = string fun get x = x)

	fun makeTestSeq (WildPat _, _, rest, mapping) = (rest, mapping)
	  | makeTestSeq (LitPat (_, lit), pos, rest, mapping) =
	    (Test (pos, LitTest lit)::rest, mapping)
	  | makeTestSeq (VarPat (_, id), pos, rest, mapping) =
	    (rest, (pos, id)::mapping)
	  | makeTestSeq (ConPat (_, longid, patOpt), pos, rest, mapping) =
	    (case patOpt of
		 SOME pat =>
		     makeTestSeq (pat, ""::pos,
				  Test (pos, ConTest (longid, true))::rest,
				  mapping)
	       | NONE => (Test (pos, ConTest (longid, false))::rest, mapping))
	  | makeTestSeq (RefPat (_, pat), pos, rest, mapping) =
	    makeTestSeq (pat, ""::pos, Test (pos, RefTest)::rest, mapping)
	  | makeTestSeq (TupPat (_, pats), pos, rest, mapping) =
	    foldlind (fn (i, pat, (rest, mapping)) =>
		      makeTestSeq (pat, Int.toString i::pos, rest, mapping))
	    (Test (pos, TupTest (List.length pats))::rest, mapping)
	    pats
	  | makeTestSeq (RowPat (_, patFields, true), pos, rest, mapping) =
	    List.foldl (fn (Field (_, Lab (_, s), pat), (rest, mapping)) =>
			makeTestSeq (pat, s::pos, rest, mapping))
	    (List.foldl (fn (Field (_, Lab (_, s), _), rest) =>
			 Test (pos, LabTest s)::rest) rest patFields,
	     mapping) patFields
	  | makeTestSeq (RowPat (_, patFields, false), pos, rest, mapping) =
	    let
		val labs =
		    List.map (fn Field (_, Lab (_, s), _) => s) patFields
		val test =
		    case StringLabelSort.sort labs of
			(_, StringLabelSort.Tup i) => TupTest i
		      | (labs', StringLabelSort.Rec) => RecTest labs
	    in
		List.foldl (fn (Field (_, Lab (_, s), pat), (rest, mapping)) =>
			    makeTestSeq (pat, s::pos, rest, mapping))
		(Test (pos, test)::rest, mapping) patFields
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
	    let
		val (rest', mapping') = makeTestSeq (pat, pos, rest, mapping)
	    in
		(Test (pos, GuardTest (mapping', exp))::rest', mapping')
	    end
	  | makeTestSeq (WithPat (coord, pat, decs), pos, rest, mapping) =
	    let
		val (rest', mapping') = makeTestSeq (pat, pos, rest, mapping)
	    in
		(Test (pos, DecTest (mapping', coord, decs))::rest', mapping')
	    end

	(* Test Graphs *)

	datatype testGraph =
	    Node of pos * test * testGraph ref * testGraph ref *
		    testList ref * testList ref * int ref * O.exp option ref
	  | Leaf of O.exp * int ref * O.exp option ref
	  | Default
	withtype testList = (pos * test) list option

	(* Construction of Test Trees Needing Backtracking *)

	fun testEq (LitTest lit1, LitTest lit2) = lit1 = lit2
	  | testEq (ConTest (longid1, hasArgs1), ConTest (longid2, hasArgs2)) =
	    longidEq (longid1, longid2)   (*--** too restrictive *)
	    andalso hasArgs1 = hasArgs2
	  | testEq (TupTest n1, TupTest n2) = n1 = n2
	  | testEq (RecTest labs1, RecTest labs2) = labs1 = labs2
	  | testEq (_, _) = false

	fun areParallelTests (LitTest lit1, LitTest lit2) = lit1 <> lit2
	  | areParallelTests (LitTest _, TupTest _) = true
	  | areParallelTests (TupTest _, LitTest _) = true
	  | areParallelTests (LitTest _, RecTest _) = true
	  | areParallelTests (RecTest _, LitTest _) = true
	  | areParallelTests (TupTest n1, TupTest n2) = n1 <> n2
	  | areParallelTests (TupTest _, RecTest _) = true
	  | areParallelTests (RecTest _, TupTest _) = true
	  | areParallelTests (RecTest labs1, RecTest labs2) = labs1 <> labs2
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
	    Crash.crash "MatchCompilationPhase.propagateElses"

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
		Crash.crash "MatchCompilationPhase.computeTestSets"
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
	  | optimizeGraph _ = Crash.crash "MatchCompilationPhase.optimizeGraph"

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
	  | countShared Default =
	    Crash.crash "MatchCompilationPhase.countShared"

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
	  | lookup (pos, nil) = Crash.crash "MatchCompilationPhase.lookup"

	fun mappingsToSubst (mapping0, mapping) =
	    List.map (fn (pos, id) => (id, lookup (pos, mapping))) mapping0

	fun makeRaise (coord, longid) =
	    O.RaiseExp (coord, O.VarExp (coord, longid))

	fun share exp =
	    (*--** replace Source.nowhere by O.infoExp exp *)
	    O.SharedExp (Source.nowhere, exp, ref O.backendInfoDummy)

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

	(*--** can this be improved? *)
	structure ExpFieldSort =
	    LabelSort(type t = exp field
		      fun get (Field (_, Lab (_, s), _)) = s)
	structure PatFieldSort =
	    LabelSort(type t = pat field
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
	    (case PatFieldSort.sort fieldPats of
		 (_, PatFieldSort.Tup i) =>
		     if i = length pats then
			 TupPat (coord,
				 ListPair.map (fn (pat1, Field (_, _, pat2)) =>
					       unify (pat1, pat2))
				 (pats, fieldPats))
		     else Error.error (coord, "pattern never matches")
	       | (_, PatFieldSort.Rec) =>
		     if hasDots then
			 Crash.crash
			 "MatchCompilationPhase.unify: not yet implemented 1"
		     else Error.error (coord, "pattern never matches"))
	  | unify (pat1 as RowPat (_, _, _), pat2 as TupPat (_, _)) =
	    unify (pat2, pat1)
	  | unify (RowPat (coord, fieldPats1, false),
		   RowPat (_, fieldPats2, false)) =
	    let
		val (fieldPats1', _) = PatFieldSort.sort fieldPats1
		val (fieldPats2', _) = PatFieldSort.sort fieldPats2
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
	  | patToExp (ConPat (coord, longid, NONE)) = ConExp (coord, longid)
	  | patToExp (ConPat (coord, longid, SOME pat)) =
	    AppExp (coord, ConExp (coord, longid), patToExp pat)
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
	  | derec (ConPat (coord, longid1, NONE), ConExp (_, longid2)) =
	    if longidEq (longid1, longid2) then nil   (*--** too restrictive *)
	    else Error.error (coord, "pattern never matches")
	  | derec (ConPat (coord, longid1, SOME pat),
		   AppExp (_, ConExp (_, longid2), exp)) =
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
	    (case ExpFieldSort.sort expFields of
		 (_, ExpFieldSort.Tup n) =>
		     if length pats = n then
			 List.foldr (fn (Field (_, Lab (_, s), exp), rest) =>
				     let
					 val i = valOf (Int.fromString s) - 1
				     in
					 derec (List.nth (pats, i), exp) @ rest
				     end) nil expFields
		     else Error.error (coord, "pattern never matches")
	       | (_, ExpFieldSort.Rec) =>
		     Error.error (coord, "pattern never matches"))
	  | derec (RowPat (coord, patFields, hasDots), TupExp (_, exps)) =
	    let
		val (patFields', arity) = PatFieldSort.sort patFields
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
			PatFieldSort.Tup i =>
			    if i = n then
				ListPair.foldr
				(fn (Field (_, _, pat), exp, rest) =>
				 derec (pat, exp) @ rest) nil
				(patFields', exps)
			    else Error.error (coord, "pattern never matches")
		      | PatFieldSort.Rec =>
			    Error.error (coord, "pattern never matches")
	    end
	  | derec (RowPat (coord, patFields, hasDots), RowExp (_, expFields)) =
	    let
		val (patFields', _) = PatFieldSort.sort patFields
		val (expFields', _) = ExpFieldSort.sort expFields
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
		fun simplifyCon decs =
		    simplifyDecs
		    (List.filter (fn dec =>
				  case dec of
				      ConDec (_, _, _) => true
				    | _ => false) decs)
		fun simplifyRec (ValDec (_, pat, exp), rest) =
		    derec (pat, exp) @ rest
		  | simplifyRec (RecDec (_, decs), rest) =
		    List.foldr simplifyRec rest decs
		  | simplifyRec (ConDec (_, _, _), rest) = rest
		val idsExpList =
		    List.map (fn (ids, exp) => (ids, simplifyExp exp))
		    (List.foldr simplifyRec nil decs)
	    in
		simplifyCon decs @
		O.RecDec (coord, idsExpList)::simplifyDecs decr
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
	  | simplifyExp (ConExp (coord, longid)) =
	    O.ConExp (coord, longid, NONE)
	  | simplifyExp (RefExp coord) =
	    O.ConExp (coord, longid_ref, NONE)
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
	  | simplifyExp (FunExp (coord, id, exp)) =
	    (*--** name propagation, multiple argument optimization *)
	    O.FunExp (coord, "", [(O.OneArg id, simplifyExp exp)])
	  | simplifyExp (AppExp (coord, exp1, exp2)) =
	    let
		val (decOpt, longid) = simplifyTerm exp1
		val exp' =
		    O.AppExp (coord, longid, simplifyExp exp2, ref false)
	    in
		case decOpt of
		    NONE => exp'
		  | SOME dec' => O.LetExp (infoExp exp1, [dec'], exp')
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
		val raiseExp = RaiseExp (coord, VarExp (coord, longid))
		val reraise = Match (coord, WildPat coord, raiseExp)
		val exp' = CaseExp (coord, VarExp (coord, longid),
				    matches @ [reraise])
	    in
		O.HandleExp (coord, simplifyExp exp, id, simplifyExp exp')
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
		  | SOME dec' => O.LetExp (infoExp exp, [dec'], exp')
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
		    foldlind (fn (i, id, mapping) =>
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
	  | simplifyTest ((GuardTest (_, _) | DecTest (_, _, _)), _, _) =
	    Crash.crash "MatchCompilationPhase.simplifyTest"

	fun simplify (decs, ids) = (simplifyDecs decs, ids)
    end
