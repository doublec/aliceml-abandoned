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
	structure O = ImperativeGrammar

	open I
	open IntermediateAux

	(* Tests *)

	datatype test =
	    LitTest of lit
	  | ConTest of longid * bool   (* has args *)
	  | RefTest
	  | TupTest of int
	  | RecTest of string list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of string
	  | VecTest of int
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
		      | (labs', StringLabelSort.Rec) => RecTest labs'
	    in
		List.foldl (fn (Field (_, Lab (_, s), pat), (rest, mapping)) =>
			    makeTestSeq (pat, s::pos, rest, mapping))
		(Test (pos, test)::rest, mapping) patFields
	    end
	  | makeTestSeq (VecPat (_, pats), pos, rest, mapping) =
	    foldli (fn (i, pat, (rest, mapping)) =>
		    makeTestSeq (pat, Int.toString i::pos, rest, mapping))
	    (Test (pos, VecTest (List.length pats))::rest, mapping)
	    pats
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
	    Node of pos * test * testGraph ref * testGraph ref * nodeStatus ref
	  | Leaf of O.body * O.body option ref
	  | Default
	and nodeStatus =
	    Initial
	  | Raw of testGraph list * testGraph list
	  | Cooked of (pos * test) list * (pos * test) list
	  | Optimized of (pos * test) list * (pos * test) list
	  | Translated of O.body

	(* Construction of Test Trees Needing Backtracking *)

	fun labEq (Lab (_, s1), Lab (_, s2)) = s1 = s2

	fun longidEq (ShortId (_, id1), ShortId (_, id2)) = idEq (id1, id2)
	  | longidEq (LongId (_, longid1, lab1), LongId (_, longid2, lab2)) =
	    longidEq (longid1, longid2) andalso labEq (lab1, lab2)
	  | longidEq (_, _) = false

	fun testEq (LitTest lit1, LitTest lit2) = lit1 = lit2
	  | testEq (ConTest (longid1, hasArgs1), ConTest (longid2, hasArgs2)) =
	    (* approximation: consider constructors equal if same long id *)
	    longidEq (longid1, longid2) andalso hasArgs1 = hasArgs2
	  | testEq (TupTest n1, TupTest n2) = n1 = n2
	  | testEq (RecTest labs1, RecTest labs2) = labs1 = labs2
	  | testEq (VecTest n1, VecTest n2) = n1 = n2
	  | testEq (_, _) = false

	fun areParallelTests (LitTest lit1, LitTest lit2) = lit1 <> lit2
	  | areParallelTests (LitTest _, TupTest _) = true
	  | areParallelTests (TupTest _, LitTest _) = true
	  | areParallelTests (LitTest _, RecTest _) = true
	  | areParallelTests (RecTest _, LitTest _) = true
	  | areParallelTests (LitTest _, VecTest _) = true
	  | areParallelTests (VecTest _, LitTest _) = true
	  | areParallelTests (TupTest n1, TupTest n2) = n1 <> n2
	  | areParallelTests (TupTest _, RecTest _) = true
	  | areParallelTests (RecTest _, TupTest _) = true
	  | areParallelTests (VecTest _, TupTest _) = true
	  | areParallelTests (TupTest _, VecTest _) = true
	  | areParallelTests (VecTest _, RecTest _) = true
	  | areParallelTests (RecTest _, VecTest _) = true
	  | areParallelTests (RecTest labs1, RecTest labs2) = labs1 <> labs2
	  | areParallelTests (VecTest n1, VecTest n2) = n1 <> n2
	  | areParallelTests (_, _) = false

	local
	    fun findTest (Node (pos', test', thenTreeRef, elseTreeRef, _),
			  pos, test) =
		if pos = pos' then
		    if testEq (test, test') then SOME thenTreeRef
		    else if areParallelTests (test, test') then
			findTest (!elseTreeRef, pos, test)
		    else NONE
		else NONE
	      | findTest (_, _, _) = NONE
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
				   ref Initial)
			 end)
	      | mergeIntoTree (Neg testSeq::testSeqRest, thenTree, elseTree) =
		mergeIntoTree (testSeq, elseTree,
			       mergeIntoTree (testSeqRest, thenTree, elseTree))
	      | mergeIntoTree (Alt testSeqs::testSeqRest, thenTree, elseTree) =
		let
		    val newThenTree =
			mergeIntoTree (testSeqRest, thenTree, Default)
		in
		    List.foldr (fn (testSeq, elseTree) =>
				mergeIntoTree (testSeq, newThenTree, elseTree))
		    elseTree testSeqs
		end
	end

	(* Elimination of Backtracking, Producing a Test Graph *)

	fun propagateElses (Node (_, _, thenTreeRef, elseTreeRef, _),
			    defaultTree) =
	    (case !elseTreeRef of
		 Default => elseTreeRef := defaultTree
	       | elseTree => propagateElses (elseTree, defaultTree);
	     case !thenTreeRef of
		 Default => thenTreeRef := defaultTree
	       | thenTree => propagateElses (thenTree, !elseTreeRef))
	  | propagateElses (Leaf (_, _), _) = ()
	  | propagateElses (Default, _) =
	    Crash.crash "SimplifyMatch.propagateElses"

	(* Optimization of the Test Graph *)

	local
	    fun union (NONE, gs) = gs
	      | union (SOME g, gr) = g::gr

	    fun computeRaw (graph as Node (_, _, ref thenGraph, ref elseGraph,
					   status as ref Initial),
			    prevTrueOpt, prevFalseOpt) =
		(status := Raw (union (prevTrueOpt, nil),
				union (prevFalseOpt, nil));
		 computeRaw (thenGraph, SOME graph, NONE);
		 computeRaw (elseGraph, NONE, SOME graph))
	      | computeRaw (Node (_, _, _, _, status as
				  ref (Raw (trueGraphs, falseGraphs))),
			    prevTrueOpt, prevFalseOpt) =
		status := Raw (union (prevTrueOpt, trueGraphs),
			       union (prevFalseOpt, falseGraphs))
	      | computeRaw (_, _, _) = ()

	    fun testSetMember (pos, test, (pos', test')::testSetRest) =
		pos = pos' andalso testEq (test, test')
		orelse testSetMember (pos, test, testSetRest)
	      | testSetMember (_, _, nil) = false

	    fun testSetIntersect ((pos, test)::testSetRest, testSet') =
		if testSetMember (pos, test, testSet') then
		    (pos, test)::(testSetIntersect (testSetRest, testSet'))
		else testSetIntersect (testSetRest, testSet')
	      | testSetIntersect (nil, _) = nil

	    fun getSets (status as ref (Raw (trueGraphs, falseGraphs))) =
		let
		    val sets = (makePosTestList (trueGraphs, true),
				makePosTestList (falseGraphs, false))
		in
		    status := Cooked sets; sets
		end
	      | getSets (ref (Cooked sets)) = sets
	      | getSets (ref (Optimized sets)) = sets
	      | getSets (ref _) = Crash.crash "SimplifyMatch.getSets"
	    and makePosTestList (graphs, isTrue) =
		List.foldr
		(fn (graph, posTestList) =>
		 case graph of
		     Node (pos, test, _, _, status) =>
			 let
			     val (trueSet, falseSet) = getSets status
			 in
			     if isTrue then
				 testSetIntersect
				 ((pos, test)::trueSet, falseSet)
			     else
				 testSetIntersect
				 (trueSet, (pos, test)::falseSet)
			 end
		   | _ => Crash.crash "SimplifyMatch.cook")
		nil graphs

	    fun disentailed (pos, test, (pos', test')::rest) =
		pos = pos' andalso areParallelTests (test, test')
		orelse disentailed (pos, test, rest)
	      | disentailed (_, _, nil) = false

	    fun optimize (ref (Node (_, _, _, _, ref (Optimized (_, _))))) = ()
	      | optimize (graphRef as
			  ref (Node (pos, test, thenGraphRef, elseGraphRef,
				     status))) =
		let
		    val sets as (trueSet, falseSet) = getSets status
		in
		    if testSetMember (pos, test, trueSet) then
			(graphRef := !thenGraphRef; optimize graphRef)
		    else if testSetMember (pos, test, falseSet)
			orelse disentailed (pos, test, trueSet) then
			(graphRef := !elseGraphRef; optimize graphRef)
		    else
			(status := Optimized sets;
			 optimize thenGraphRef;
			 optimize elseGraphRef)
		end
	      | optimize (ref (Leaf (_, _))) = ()
	      | optimize (ref _) = Crash.crash "SimplifyMatch.optimize"
	in
	    fun optimizeGraph graph =
		let
		    val _ = computeRaw (graph, NONE, NONE)
		    val graphRef = ref graph
		in
		    optimize graphRef; !graphRef
		end
	end

	type consequent = (O.coord * O.body option ref)

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
				    val leaf = Leaf (thenExp, r)
				in
				    (mergeIntoTree (List.rev testSeq,
						    leaf, elseTree),
				     (coord, r)::consequents)
				end) (Default, nil) matches
		val elseGraph = Leaf (elseExp, ref NONE)
	    in
		case graph of
		    Default =>
			(elseGraph, consequents)
		  | _ =>
			(propagateElses (graph, elseGraph);
			 (optimizeGraph graph, consequents))
	    end

	local
	    datatype args =
		ONE
	      | TUP of int
	      | REC of string list

	    exception NonArgable

	    fun normalize (_, LitPat (_, _), _) = ONE
	      | normalize (_, ConPat (_, _, _), _) = ONE
	      | normalize (_, RefPat (_, _), _) = ONE
	      | normalize (_, TupPat (_, pats), _) = TUP (List.length pats)
	      | normalize (_, RowPat (_, patFields, true), _) =
		let
		    val labs =
			List.map (fn Field (_, Lab (_, s), _) => s) patFields
		in
		    case StringLabelSort.sort labs of
			(_, StringLabelSort.Tup i) => TUP i
		      | (labs', StringLabelSort.Rec) => REC labs'
		end
	      | normalize (_, VecPat (_, _), _) = ONE
	      | normalize (_, _, _) = raise NonArgable

	    fun insertMatch ((ONE, matches)::rest, ONE, match) =
		(ONE, match::matches)::rest
	      | insertMatch (argsMatchesList, ONE, match) =
		(ONE, [match])::argsMatchesList
	      | insertMatch ((args, matches)::rest, args', match) =
		if args = args' then (args, match::matches)::rest
		else (args, matches)::insertMatch (rest, args', match)
	      | insertMatch (nil, args', match) = [(args', [match])]

	    fun makeArg (match, argsMatchesList) =
		insertMatch (argsMatchesList, normalize match, match)

	    fun freshId coord = Id (coord, Stamp.new (), InId)

	    fun process (ONE, graph, consequents, id) =
		(O.OneArg id, graph, [(nil, id)], consequents)
	      | process (TUP i, Node (nil, TupTest i', ref graph, _, _),
			 consequents, _) =
		let
		    val intIdList =
			List.tabulate
			(i, fn i => (i + 1, freshId Source.nowhere))
		    val ids = List.map #2 intIdList
		    val mapping =
			List.foldr (fn ((i, id), mapping) =>
				    ([Int.toString i], id)::mapping)
			nil intIdList
		in
		    if i = i' then ()
		    else Crash.crash "SimplifyMatch.process 1";
		    (O.TupArgs ids, graph, mapping, consequents)
		end
	      | process (REC labs, Node (nil, RecTest labs', ref graph, _, _),
			 consequents, _) =
		let
		    val labIdList =
			List.map (fn lab => (lab, freshId Source.nowhere)) labs
		    val mapping =
			List.foldr (fn ((lab, id), mapping) =>
				    ([lab], id)::mapping) nil labIdList
		in
		    if labs = labs' then ()
		    else Crash.crash "SimplifyMatch.process 2";
		    (O.RecArgs labIdList, graph, mapping, consequents)
		end
	      | process (_, _, _, _) = Crash.crash "SimplifyMatch.process"
	in
	    fun buildFunArgs (id, matches, errStms) =
		let
		    val argsMatchesList =
			(List.foldl makeArg nil matches)
			handle NonArgable => [(ONE, matches)]
		in
		    List.map (fn (args, matches) =>
			      let
				  val (graph, consequents) =
				      buildGraph (matches, errStms)
			      in
				  process (args, graph, consequents, id)
			      end) argsMatchesList
		end
	end
    end
