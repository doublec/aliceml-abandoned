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
	structure Intermediate = PostTranslationIntermediate
	structure Simplified = Simplified

	open PostTranslationIntermediate
	structure S = Simplified

	(* Tests *)

	datatype test =
	    LitTest of lit
	  | NameTest of longid
	  | ConTest of longid
	  | RecTest of arity
	  | LabelTest of string
	  | GuardTest of exp
	  | DecTest of dec list
	and arity =
	    TupArity of int
	  | RecArity of string list   (* sorted, no tuple *)

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
	    fun tupArity (s::sr, i) =
		if s = Int.toString i then tupArity (sr, i + 1)
		else NONE
	      | tupArity (nil, i) = SOME (i - 1)
	in
	    fun makeArity ss =
		let
		    val ss' = LabelSort.sort ss
		in
		    case tupArity (ss', 1) of
			NONE => RecArity ss'
		      | SOME i => TupArity i
		end
	end

	fun makeTestSeq (WildPat _, _, rest) = rest
	  | makeTestSeq (LitPat (_, lit), pos, rest) =
	    Test (pos, LitTest lit)::rest
	  | makeTestSeq (VarPat (_, _), _, rest) = rest
	  | makeTestSeq (ConPat (_, longid, patOpt), pos, rest) =
	    (case patOpt of
		 SOME pat => makeTestSeq (pat, ""::pos,
					  Test (pos, ConTest longid)::rest)
	       | NONE => Test (pos, NameTest longid)::rest)
	  | makeTestSeq (TupPat (_, pats), pos, rest) =
	    foldlind (fn (i, pat, rest) =>
		      makeTestSeq (pat, Int.toString i::pos, rest))
	    (Test (pos, RecTest (TupArity (List.length pats)))::rest) pats
	  | makeTestSeq (RecPat (_, patFields, true), pos, rest) =
	    foldl (fn (Field (_, Lab (_, s), pat), rest) =>
		   makeTestSeq (pat, s::pos, rest))
	    (foldl (fn (Field (_, Lab (_, s), _), rest) =>
		    Test (pos, LabelTest s)::rest) rest patFields) patFields
	  | makeTestSeq (RecPat (_, patFields, false), pos, rest) =
	    let
		val arity =
		    makeArity (List.map (fn Field (_, Lab (_, s), _) => s)
			       patFields)
	    in
		foldl (fn (Field (_, Lab (_, s), pat), rest) =>
		       makeTestSeq (pat, s::pos, rest))
		(Test (pos, RecTest arity)::rest) patFields
	    end
	  | makeTestSeq (AsPat (_, pat1, pat2), pos, rest) =
	    makeTestSeq (pat2, pos, makeTestSeq (pat1, pos, rest))
	  | makeTestSeq (AltPat (_, pats), pos, rest) =
	    Alt (List.map (fn pat => List.rev (makeTestSeq (pat, pos, nil)))
		 pats)::rest
	  | makeTestSeq (NegPat (_, pat), pos, rest) =
	    Neg (List.rev (makeTestSeq (pat, pos, nil)))::rest
	  | makeTestSeq (GuardPat (_, pat, exp), pos, rest) =
	    makeTestSeq (pat, pos, Test (pos, GuardTest exp)::rest)
	  | makeTestSeq (WithPat (_, pat, decs), pos, rest) =
	    Test (pos, DecTest decs)::makeTestSeq (pat, pos, rest)

	(* Test Graphs *)

	datatype testGraph =
	    Node of pos * test * testGraph ref * testGraph ref *
		    testList ref * testList ref * int ref * S.exp option ref
	  | Leaf of exp * int ref * S.exp option ref
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
	    Crash.crash "PatternCompiler.propagateElses"

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
		Crash.crash "PatternCompiler.computeTestSets"
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
	  | optimizeGraph _ = Crash.crash "PatternCompiler.optimizeGraph"

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
	  | countShared Default = Crash.crash "PatternCompiler.countShared"

	fun makeLeaf exp = Leaf (exp, ref 0, ref NONE)

	fun buildGraph (matches, elseExp) =
	    let
		val graph =
		    List.foldr (fn (Match (_, pat, thenExp), elseTree) =>
				let
				    val testSeq = makeTestSeq (pat, nil, nil)
				in
				    mergeIntoTree (List.rev testSeq,
						   makeLeaf thenExp, elseTree)
				end) Default matches
	    in
		case graph of
		    Default => makeLeaf elseExp
		  | _ => (propagateElses (graph, makeLeaf elseExp);
			  computeTestSets (graph, nil, nil);
			  let
			      val graphRef = ref graph
			  in
			      optimizeGraph graphRef;
			      countShared (!graphRef);
			      !graphRef
			  end)
	    end

	(* Generate Simplified Ouput from Graph *)

	type mapping = (pos * id) list

	fun lookup (pos, (pos', id)::mappingRest) =
	    if pos = pos' then id
	    else lookup (pos, mappingRest)
	  | lookup (pos, nil) = Crash.crash "PatternCompiler.lookup"

	fun freshId coord = Id (coord, Stamp.new (), InId)

	fun makeRaise (coord, longid) =
	    S.RaiseExp (coord, S.VarExp (coord, longid))

	fun share exp =
	    S.SharedExp (Source.nowhere, exp, ref Simplified.backendInfoDummy)

	fun makeShared (exp, ref 1) = exp
	  | makeShared (exp, ref _) = share exp

	fun simplifyDec (ValDec (coord, ids, exp)) =
	    S.ValRecDec (coord, ids, simplifyExp exp)
	  | simplifyDec (ConDec (coord, id, hasArgs)) =
	    S.ConDec (coord, id, hasArgs)
	and simplifyExp (LitExp (coord, lit)) =
	    S.LitExp (coord, lit)
	  | simplifyExp (VarExp (coord, longid)) =
	    S.VarExp (coord, longid)
	  | simplifyExp (ConExp (coord, longid, SOME exp)) =
	    let
		val id' = freshId Source.nowhere
		val longid' = ShortId (Source.nowhere, id')
		val dec' = S.ValDec (coord, id', simplifyExp exp)
	    in
		S.LetExp (Source.nowhere, [dec'],
			  S.ConExp (coord, longid, SOME longid'))
	    end
	  | simplifyExp (ConExp (coord, longid, NONE)) =
	    S.ConExp (coord, longid, NONE)
	  | simplifyExp (TupExp (coord, exps)) =
	    let
		val ids = List.map (fn _ => freshId Source.nowhere) exps
		val decs' =
		    ListPair.map (fn (id, exp) =>
				  S.ValDec (Source.nowhere, id,
					    simplifyExp exp)) (ids, exps)
		val longids =
		    List.map (fn id => ShortId (Source.nowhere, id)) ids
	    in
		S.LetExp (Source.nowhere, decs', S.TupExp (coord, longids))
	    end
	  | simplifyExp (RecExp (coord, expFields)) =
	    let
		val ids = List.map (fn _ => freshId Source.nowhere) expFields
		val decs' =
		    ListPair.map (fn (id, Field (_, _, exp)) =>
				  S.ValDec (Source.nowhere, id,
					    simplifyExp exp)) (ids, expFields)
		val fields =
		    ListPair.map (fn (id, Field (_, lab, _)) =>
				  (lab, ShortId (Source.nowhere, id)))
		    (ids, expFields)
	    in
		S.LetExp (Source.nowhere, decs', S.RecExp (coord, fields))
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
	    simplifyIf (exp1, simplifyExp exp2, simplifyExp exp3,
			makeRaise (Source.nowhere, longid_Match))
	  | simplifyExp (WhileExp (coord, exp1, exp2)) =
	    S.WhileExp (coord, simplifyExp exp1, simplifyExp exp2)
	  | simplifyExp (SeqExp (coord, exps)) =
	    S.SeqExp (coord, List.map simplifyExp exps)
	  | simplifyExp (CaseExp (coord, exp, matches, longid)) =
	    let
		val id' = freshId Source.nowhere
		val dec' = S.ValDec (coord, id', simplifyExp exp)
(*--**		val errExp' = RaiseExp (coord, VarExp (coord, longid))*)
	    in
		S.LetExp (Source.nowhere, [dec'],
			  simplifyGraph (buildGraph (matches, exp),
					 [(nil, id')]))
	    end
	  | simplifyExp (RaiseExp (coord, exp)) =
	    S.RaiseExp (coord, simplifyExp exp)
	  | simplifyExp (HandleExp (coord, exp1, id, exp2)) =
	    S.HandleExp (coord, simplifyExp exp1, id, simplifyExp exp2)
	  | simplifyExp (LetExp (coord, decs, exp)) =
	    S.LetExp (coord, List.map simplifyDec decs, simplifyExp exp)
	and simplifyIf (AndExp (_, exp1, exp2), thenExp, elseExp, errExp) =
	    let
		val elseExp' = share elseExp
		val thenExp' = simplifyIf (exp2, thenExp, elseExp', errExp)
		val errExp' = makeRaise (Source.nowhere, longid_Match)
	    in
		simplifyIf (exp1, thenExp', elseExp', errExp')
	    end
	  | simplifyIf (OrExp (_, exp1, exp2), thenExp, elseExp, errExp) =
	    let
		val thenExp' = share thenExp
		val elseExp' = simplifyIf (exp2, thenExp', elseExp, errExp)
		val errExp' = makeRaise (Source.nowhere, longid_Match)
	    in
		simplifyIf (exp1, thenExp', elseExp', errExp')
	    end
	  | simplifyIf (exp, thenExp, elseExp, errExp) =
	    let
		val id = freshId Source.nowhere
		val longid = ShortId (Source.nowhere, id)
		val dec' = S.ValDec (Source.nowhere, id, simplifyExp exp)
	    in
		S.LetExp (Source.nowhere, [dec'],
			  S.TestExp (Source.nowhere, longid,
				     S.NameTest longid_true, thenExp,
				     S.TestExp (Source.nowhere, longid,
						S.NameTest longid_false,
						elseExp, errExp)))
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
	  | simplifyGraph (Leaf (exp, count, expOptRef as ref NONE), mapping) =
	    let
		val exp' = makeShared (simplifyExp exp, count)
	    in
		expOptRef := SOME exp';
		exp'
	    end
	  | simplifyGraph (Default, _) =
	    Crash.crash "PatternCompiler.simplifyGraph"
	and simplifyNode (pos, GuardTest exp, thenGraph, elseGraph, mapping) =
	    let
		val coord = Source.nowhere
		val id = freshId coord
		val longid = ShortId (coord, id)
		val dec' = S.ValDec (coord, id, simplifyExp exp)
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
	  | simplifyNode (pos, DecTest decs, thenGraph, _, mapping) =
	    let
		val decs' = List.map simplifyDec decs
		val thenExp = simplifyGraph (thenGraph, mapping)
	    in
		S.LetExp (Source.nowhere, decs', thenExp)
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
	  | simplifyTest (LabelTest string, pos, mapping) =
	    let
		val id = freshId Source.nowhere
		val mapping' = ((string::pos), id)::mapping
	    in
		(S.LabelTest (string, id), mapping')
	    end
	  | simplifyTest ((GuardTest _ | DecTest _), _, _) =
	    Crash.crash "PatternCompiler.simplifyTest"
    end
