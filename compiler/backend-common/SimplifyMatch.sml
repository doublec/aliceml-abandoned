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

	type pos = Label.t list
	type typ = Type.t

	datatype test =
	    LitTest of I.lit
	  | ConTest of I.longid * typ option   (* has args *)
	  | RefTest of typ
	  | TupTest of typ list
	  | RecTest of (Label.t * typ) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of Label.t * typ
	  | VecTest of typ list
	  | GuardTest of mapping * I.exp
	  | DecTest of mapping * I.dec list
	withtype mapping = (pos * id) list

	(* Test Sequences *)

	datatype testSeqElem =
	    Test of pos * test
	  | Neg of testSeq
	  | Alt of testSeq list
	withtype testSeq = testSeqElem list

	(* Test Sequence Construction *)

	structure LabelSort =
	    MakeLabelSort(type 'a t = Label.t * 'a
			  fun get (label, _) = label)

	fun typPat pat = valOf (IntermediateInfo.typ (infoPat pat))

	fun longidToLabel (ShortId (_, Id (_, _, Name.ExId s))) =
	    Label.fromString s
	  | longidToLabel (ShortId (_, Id (_, _, Name.InId))) =
	    raise Crash.Crash "SimplifyMatch.longIdToLabel"
	  | longidToLabel (LongId (_, _, Lab (_, label))) = label

	fun makeTestSeq (WildPat _, _, rest, mapping) = (rest, mapping)
	  | makeTestSeq (LitPat (_, lit), pos, rest, mapping) =
	    (Test (pos, LitTest lit)::rest, mapping)
	  | makeTestSeq (VarPat (_, id), pos, rest, mapping) =
	    (rest, (pos, id)::mapping)
	  | makeTestSeq (ConPat (_, longid, patOpt, _), pos, rest, mapping) =
	    (case patOpt of
		 SOME pat =>
		     makeTestSeq (pat, longidToLabel longid::pos,
				  Test (pos,
					ConTest (longid, SOME (typPat pat)))::
				  rest, mapping)
	       | NONE => (Test (pos, ConTest (longid, NONE))::rest, mapping))
	  | makeTestSeq (RefPat (_, pat), pos, rest, mapping) =
	    makeTestSeq (pat, Label.fromString "ref"::pos,
			 Test (pos, RefTest (typPat pat))::rest, mapping)
	  | makeTestSeq (TupPat (_, pats), pos, rest, mapping) =
	    Misc.List_foldli
	    (fn (i, pat, (rest, mapping)) =>
	     makeTestSeq (pat, Label.fromInt (i + 1)::pos, rest, mapping))
	    (Test (pos, TupTest (List.map typPat pats))::rest, mapping)
	    pats
	  | makeTestSeq (RowPat (info, patFields), pos, rest, mapping) =
	    let
		val row = Type.asRow (valOf (IntermediateInfo.typ info))
		fun convert row =
		    if Type.isEmptyRow row then (nil, Type.isUnknownRow row)
		    else
			let
			    val (rest, hasDots) = convert (Type.tailRow row)
			in
			    case Type.headRow row of
				(label, [typ]) => ((label, typ)::rest, hasDots)
			      | _ =>
				    raise Crash.Crash
					"SimplifyMatch.makeTestSeq"
			end
		val (labelTypList, hasDots) = convert row
	    in
		if hasDots then
		    List.foldl (fn (Field (_, Lab (_, s), pat),
				    (rest, mapping)) =>
				makeTestSeq (pat, s::pos, rest, mapping))
		    (List.foldl (fn (Field (_, Lab (_, l), pat), rest) =>
				 Test (pos, LabTest (l, typPat pat))::rest)
		     rest patFields, mapping) patFields
		else
		    let
			val test =
			    case LabelSort.sort labelTypList of
				(labelTypList', LabelSort.Tup _) =>
				    TupTest (List.map #2 labelTypList')
			      | (labelTypList', LabelSort.Rec) =>
				    RecTest labelTypList'
		    in
			List.foldl (fn (Field (_, Lab (_, s), pat),
					(rest, mapping)) =>
				    makeTestSeq (pat, s::pos, rest, mapping))
			(Test (pos, test)::rest, mapping) patFields
		    end
	    end
	  | makeTestSeq (VecPat (_, pats), pos, rest, mapping) =
	    Misc.List_foldli
	    (fn (i, pat, (rest, mapping)) =>
	     makeTestSeq (pat, Label.fromInt (i + 1)::pos, rest, mapping))
	    (Test (pos, VecTest (List.map typPat pats))::rest, mapping)
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
	  | makeTestSeq (WithPat (_, pat, decs), pos, rest, mapping) =
	    let
		val (rest', mapping') = makeTestSeq (pat, pos, rest, mapping)
	    in
		(Test (pos, DecTest (mapping', decs))::rest', mapping')
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

	fun testEq (LitTest lit1, LitTest lit2) = lit1 = lit2
	  | testEq (ConTest (longid1, _), ConTest (longid2, _)) =
	    longidToLabel longid1 = longidToLabel longid2
	  | testEq (TupTest _, _) = true
	  | testEq (RecTest _, _) = true
	  | testEq (VecTest typs1, VecTest typs2) =
	    List.length typs1 = List.length typs2
	  | testEq (_, _) = false

	fun areParallelTests (LitTest lit1, LitTest lit2) = lit1 <> lit2
	  | areParallelTests (VecTest typs1, VecTest typs2) =
	    List.length typs1 <> List.length typs2
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
	    raise Crash.Crash "SimplifyMatch.propagateElses"

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
	      | getSets (ref _) = raise Crash.Crash "SimplifyMatch.getSets"
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
		   | _ => raise Crash.Crash "SimplifyMatch.cook")
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
	      | optimize (ref _) = raise Crash.Crash "SimplifyMatch.optimize"
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

	(*
	 * Check whether the match rules of a function define
	 * a cartesian n-ary function; if they do, represent
	 * the cartesian arity explicitly.
	 *
	 * Preconditions:
	 * -- No pattern binds the whole argument value
	 *    to a variable.
	 * -- The first pattern is not a wildcard.
	 *    If it was, the function might perform some arbitrary
	 *    action before deconstructing the tuple or record.
	 *    This order would be reversed if the function was
	 *    translated to an n-ary function; however, in the
	 *    presence of transients, the deconstruction may have
	 *    a side effect.
	 *)

	type bodyFun = unit -> O.body

	local
	    datatype arity =
		ONE
	      | TUP of typ list
	      | REC of (Label.t * typ) list

	    exception MustBeUnary

	    structure LabelSort =
		MakeLabelSort(type 'a t = Label.t * 'a
			      fun get (label, _) = label)

	    fun typToArity typ =
		if Type.isTuple typ then TUP (Type.asTuple typ)
		else if Type.isRow typ then
		    let
			fun convert row =
			    if Type.isEmptyRow row then
				if Type.isUnknownRow row then raise MustBeUnary
				else nil
			    else
				(case Type.headRow row of
				     (label, [typ]) => (label, typ)
				   | (_, _) => raise MustBeUnary)::
				convert (Type.tailRow row)
		    in
			case LabelSort.sort (convert (Type.asRow typ)) of
			    (labelTypList, LabelSort.Tup _) =>
				TUP (List.map #2 labelTypList)
			  | (labelTypList, LabelSort.Rec) => REC labelTypList
		    end handle MustBeUnary => ONE
		else ONE

	    fun isManifest (WildPat _, wild) = wild
	      | isManifest (VarPat (_, _), _) = false
	      | isManifest (TupPat (_, _), _) = true
	      | isManifest (RowPat (_, _), _) = true
	      | isManifest (AsPat (_, pat1, pat2), wild) =
		isManifest (pat1, wild) andalso isManifest (pat2, wild)
	      | isManifest (AltPat (_, pats), wild) =
		List.all (fn pat => isManifest (pat, wild)) pats
	      | isManifest (NegPat (_, pat), wild) = isManifest (pat, wild)
	      | isManifest (GuardPat (_, pat, _), wild) =
		isManifest (pat, wild)
	      | isManifest (WithPat (_, pat, _), wild) = isManifest (pat, wild)
	      | isManifest (_, _) =
		raise Crash.Crash "SimplifyMatch.normalize type inconsistency"

	    fun checkMatches ((_, pat, _)::matches, wild) =
		isManifest (pat, wild) andalso checkMatches (matches, true)
	      | checkMatches (nil, _) = true

	    fun freshId info = Id (info, Stamp.new (), Name.InId)

	    fun process (ONE, graph, consequents, id) =
		(O.OneArg id, graph, [(nil, id)], consequents)
	      | process (TUP typs, Node (nil, TupTest _, ref graph, _, _),
			 consequents, _) =
		let
		    val ids =
			List.map (fn typ => freshId (Source.nowhere, SOME typ))
			typs
		    val labelIdList =
			Misc.List_mapi (fn (i, id) =>
					(Label.fromInt (i + 1), id)) ids
		    val mapping =
			List.foldr (fn ((label, id), mapping) =>
				    ([label], id)::mapping) nil labelIdList
		in
		    (O.TupArgs ids, graph, mapping, consequents)
		end
	      | process (REC labelTypList,
			 Node (nil, RecTest _, ref graph, _, _),
			 consequents, _) =
		let
		    val labelIdList =
			List.map (fn (label, typ) =>
				  (label, freshId (Source.nowhere, SOME typ)))
			labelTypList
		    val mapping =
			List.foldr (fn ((lab, id), mapping) =>
				    ([lab], id)::mapping) nil labelIdList
		in
		    (O.RecArgs labelIdList, graph, mapping, consequents)
		end
	      | process (_, _, _, _) =
		raise Crash.Crash "SimplifyMatch.process 3"
	in
	    fun buildFunArgs (id, matches as (_, pat, _)::_, errStmsFun) =
		let
		    val argsMatchesList =
			if checkMatches (matches, false) then
			    [(typToArity (typPat pat), matches)]
			else [(ONE, matches)]
		in
		    List.map (fn (args, matches) =>
			      let
				  val (graph, consequents) =
				      buildGraph (matches, errStmsFun ())
			      in
				  process (args, graph, consequents, id)
			      end) argsMatchesList
		end
	      | buildFunArgs (_, nil, _) =
		raise Crash.Crash "SimplifyMatch.buildFunArgs"
	end
    end
