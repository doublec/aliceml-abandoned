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

structure SimplifyMatch :> SIMPLIFY_MATCH =
    struct
	structure I = IntermediateGrammar
	structure O = SimplifiedGrammar

	open I
	open IntermediateAux

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

	structure StringLabelSort =
	    MakeLabelSort(type 'a t = string fun get x = x)

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
	    foldli (fn (i, pat, (rest, mapping)) =>
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
	    (* approximation: consider constructors equal if same long id *)
	    longidEq (longid1, longid2) andalso hasArgs1 = hasArgs2
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
    end
