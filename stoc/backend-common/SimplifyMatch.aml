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
	structure O = FlatGrammar

	open I
	open IntermediateAux

	(* Tests *)

	datatype selector =
	    LABEL of Label.t
	  | LONGID of Stamp.t * Label.t list
	type pos = selector list
	type typ = Type.t

	datatype test =
	    LitTest of I.lit
	  | TagTest of Label.t * typ option * O.conArity
	  | ConTest of I.longid * typ option * O.conArity
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

	fun typPat pat = #typ (infoPat pat)

	local
	    fun longidToSelector' (ShortId (_, Id (_, stamp, _))) =
		(stamp, nil)
	      | longidToSelector' (LongId (_, longid, Lab (_, label))) =
		let
		    val (stamp, labels) = longidToSelector' longid
		in
		    (stamp, label::labels)
		end
	in
	    fun longidToSelector longid =
		let
		    val (stamp, labels) = longidToSelector' longid
		in
		    LONGID (stamp, List.rev labels)
		end
	end

	local
	    fun parseRow row =
		if Type.isEmptyRow row then (nil, Type.isUnknownRow row)
		else
		    let
			val (rest, hasDots) = parseRow (Type.tailRow row)
		    in
			case Type.headRow row of
			    (label, [typ]) => ((label, typ)::rest, hasDots)
			  | (_, _) =>
				raise Crash.Crash "SimplifyMatch.parseRow"
		    end
	in
	    fun getRow typ =
		let
		    val (labelTypList, hasDots) =
			if Type.isProd typ then parseRow (Type.asProd typ)
			else
			    (Misc.List_mapi (fn (i, typ) =>
					     (Label.fromInt (i + 1), typ))
			     (Type.asTuple typ), false)
		    val (labelTypList', arity) = LabelSort.sort labelTypList
		in
		    (labelTypList', arity, hasDots)
		end
	end

	fun makeTestSeq (WildPat _, _, rest, mapping) = (rest, mapping)
	  | makeTestSeq (LitPat (_, lit), pos, rest, mapping) =
	    (Test (pos, LitTest lit)::rest, mapping)
	  | makeTestSeq (VarPat (_, id), pos, rest, mapping) =
	    (rest, (pos, id)::mapping)
	  | makeTestSeq (TagPat (info, Lab (_, label), isNAry),
			 pos, rest, mapping) =
	    let
		val info' = exp_info (Source.nowhere, #typ info)
		val conArity = makeConArity (info', isNAry)
	    in
		(Test (pos, TagTest (label, NONE, conArity))::rest, mapping)
	    end
	  | makeTestSeq (AppPat (_, TagPat (info, Lab (_, label), isNAry),
				 pat), pos, rest, mapping) =
	    let
		val typ = Type.inArrow (typPat pat, #typ info)
		val info' = exp_info (Source.nowhere, typ)
		val conArity = makeConArity (info', isNAry)
	    in
		makeTestSeq (pat, LABEL label::pos,
			     Test (pos, TagTest (label, SOME (typPat pat),
						 conArity))::rest, mapping)
	    end
	  | makeTestSeq (ConPat (info, longid, isNAry), pos, rest, mapping) =
	    let
		val info' = exp_info (Source.nowhere, #typ info)
		val conArity = makeConArity (info', isNAry)
	    in
		(Test (pos, ConTest (longid, NONE, conArity))::rest, mapping)
	    end
	  | makeTestSeq (AppPat (_, ConPat (info, longid, isNAry), pat),
			 pos, rest, mapping) =
	    let
		val typ = Type.inArrow (typPat pat, #typ info)
		val info' = exp_info (Source.nowhere, typ)
		val conArity = makeConArity (info', isNAry)
	    in
		makeTestSeq (pat, longidToSelector longid::pos,
			     Test (pos, ConTest (longid, SOME (typPat pat),
						 conArity))::rest, mapping)
	    end
	  | makeTestSeq (AppPat (_, RefPat _, pat), pos, rest, mapping) =
	    makeTestSeq (pat, LABEL (Label.fromString "ref")::pos,
			 Test (pos, RefTest (typPat pat))::rest, mapping)
	  | makeTestSeq (TupPat (_, pats), pos, rest, mapping) =
	    Misc.List_foldli
	    (fn (i, pat, (rest, mapping)) =>
	     makeTestSeq (pat, LABEL (Label.fromInt (i + 1))::pos,
			  rest, mapping))
	    (Test (pos, TupTest (List.map typPat pats))::rest, mapping) pats
	  | makeTestSeq (RowPat (info, patFields), pos, rest, mapping) =
	    List.foldl (fn (Field (_, Lab (_, label), pat), (rest, mapping)) =>
			makeTestSeq (pat, LABEL label::pos, rest, mapping))
	    (case getRow (#typ info) of
		 (labelTypList, _, true) =>
		     List.foldl (fn ((label, typ), rest) =>
				 Test (pos, LabTest (label, typ))::rest)
		     rest labelTypList
	       | (labelTypList, LabelSort.Tup _, false) =>
		     Test (pos, TupTest (List.map #2 labelTypList))::rest
	       | (labelTypList, LabelSort.Rec, false) =>
		     Test (pos, RecTest labelTypList)::rest, mapping) patFields
	  | makeTestSeq (VecPat (_, pats), pos, rest, mapping) =
	    Misc.List_foldli
	    (fn (i, pat, (rest, mapping)) =>
	     makeTestSeq (pat, LABEL (Label.fromInt (i + 1))::pos,
			  rest, mapping))
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
	  | makeTestSeq ((RefPat _ | AppPat (_, _, _)), _, _, _) =
	    raise Crash.Crash "SimplifyMatch.makeTestSeq"

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
	  | testEq (TagTest (label1, _, _), TagTest (label2, _, _)) =
	    label1 = label2
	  | testEq (ConTest (_, _, _), ConTest (_, _, _)) = false
	    (*--** use longidEq as better approximation *)
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

	type consequent = (Source.region * O.body option ref)

	fun buildGraph (matches, elseExp) =
	    let
		val (graph, consequents) =
		    List.foldr (fn ((region, pat, thenExp),
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
				     (region, r)::consequents)
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
	 * 1) No pattern binds the whole argument value to a variable.
	 * 2) No side effect can be performed by a GuardPat or WithPat
	 *    before the tuple or record is deconstructed (since in the
	 *    presence of by-need futures, the latter may also have
	 *    side effects).
	 *)

	local
	    datatype arity =
		ONE
	      | TUP of typ list
	      | REC of (Label.t * typ) list

	    exception MustBeUnary

	    local
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
		fun typToArity typ =
		    if Type.isTuple typ then TUP (Type.asTuple typ)
		    else if Type.isProd typ then
			(case LabelSort.sort (convert (Type.asProd typ)) of
			     (labelTypList, LabelSort.Tup _) =>
				 TUP (List.map #2 labelTypList)
			   | (labelTypList, LabelSort.Rec) => REC labelTypList)
			handle MustBeUnary => ONE
		else ONE
	    end

	    exception BindsAll     (* precondition 1 not satisfied *)
	    exception SideEffect   (* precondition 2 not satisfied *)
	    exception NotNAry

	    fun deconstructs (WildPat _) = false
	      | deconstructs (LitPat _) = raise NotNAry
	      | deconstructs (VarPat (_, _)) = raise BindsAll
	      | deconstructs (TagPat (_, _, _)) = raise NotNAry
	      | deconstructs (ConPat (_, _, _)) = raise NotNAry
	      | deconstructs (RefPat _) = raise NotNAry
	      | deconstructs (TupPat (_, _)) = true
	      | deconstructs (RowPat (_, _)) = true
	      | deconstructs (VecPat (_, _)) = raise NotNAry
	      | deconstructs (AppPat (_, _, _)) = raise NotNAry
	      | deconstructs (AsPat (_, pat1, pat2)) =
		deconstructs pat1 orelse deconstructs pat2
	      | deconstructs (AltPat (_, pats)) = List.exists deconstructs pats
	      | deconstructs (NegPat (_, pat)) = deconstructs pat
	      | deconstructs (GuardPat (_, pat, _)) =
		deconstructs pat orelse raise SideEffect
	      | deconstructs (WithPat (_, pat, _)) =
		deconstructs pat orelse raise SideEffect

	    fun checkMatches matches =
		(List.foldl (fn ((_, pat, _), b) =>
			     deconstructs pat orelse b) false matches)
		handle (BindsAll | SideEffect | NotNAry) => false

	    fun process (ONE, graph, consequents, info) =
		let
		    val id = freshId info
		in
		    (O.OneArg id, graph, [(nil, id)], consequents)
		end
	      | process (TUP typs, Node (nil, TupTest _, ref graph, _, _),
			 consequents, _) =
		let
		    val ids =
			List.map (fn typ =>
				  freshId (exp_info (Source.nowhere, typ)))
			typs
		    val labelIdList =
			Misc.List_mapi (fn (i, id) =>
					(Label.fromInt (i + 1), id)) ids
		    val mapping =
			List.foldr (fn ((label, id), mapping) =>
				    ([LABEL label], id)::mapping)
			nil labelIdList
		in
		    (O.TupArgs ids, graph, mapping, consequents)
		end
	      | process (REC labelTypList,
			 Node (nil, RecTest _, ref graph, _, _),
			 consequents, _) =
		let
		    val labelIdList =
			List.map (fn (label, typ) =>
				  (label,
				   freshId (exp_info (Source.nowhere, typ))))
			labelTypList
		    val mapping =
			List.foldr (fn ((label, id), mapping) =>
				    ([LABEL label], id)::mapping)
			nil labelIdList
		in
		    (O.RecArgs labelIdList, graph, mapping, consequents)
		end
	      | process (_, _, _, _) =
		raise Crash.Crash "SimplifyMatch.process"
	in
	    fun buildFunArgs (matches as (_, pat, _)::_, errStms) =
		let
		    val arity =
			if checkMatches matches then typToArity (typPat pat)
			else ONE
		    val (graph, consequents) = buildGraph (matches, errStms)
		in
		    process (arity, graph, consequents, infoPat pat)
		end
	      | buildFunArgs (nil, _) =
		raise Crash.Crash "SimplifyMatch.buildFunArgs"
	end
    end
