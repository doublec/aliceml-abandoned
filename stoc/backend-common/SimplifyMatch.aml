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
	  | TagTest of Label.t * int
	  | TagAppTest of Label.t * int * typ O.args * O.conArity
	  | ConTest of I.longid
	  | ConAppTest of I.longid * typ O.args * O.conArity
	  | RefAppTest of typ
	  | TupTest of typ list
	  | RecTest of (Label.t * typ) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of Label.t * int * typ
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
			    (List.mapi (fn (i, typ) =>
					(Label.fromInt (i + 1), typ))
			     (Type.asTuple typ), false)
		    val (labelTypList', arity) = LabelSort.sort labelTypList
		in
		    (labelTypList', arity, hasDots)
		end
	end

	fun makeAppArgs (TupPat (_, pats), true, pos) =
	    (List.mapi (fn (i, pat) =>
			(LABEL (Label.fromInt (i + 1))::pos, pat)) pats,
	     O.TupArgs (List.map typPat pats))
	  | makeAppArgs (pat as ProdPat (info, patFields), true, pos) =
	    (case getRow (#typ info) of
		 (_, _, true) => ([(pos, pat)], O.OneArg (#typ info))
	       | (labelTypList, LabelSort.Tup _, false) =>
		     (List.map (fn Field (_, Lab (_, label), pat) =>
				(LABEL label::pos, pat)) patFields,
		      O.TupArgs (List.map #2 labelTypList))
	       | (labelTypList, LabelSort.Rec, false) =>
		     (List.map (fn Field (_, Lab (_, label), pat) =>
				(LABEL label::pos, pat)) patFields,
		      O.RecArgs labelTypList))
	  | makeAppArgs (pat, _, pos) = ([(pos, pat)], O.OneArg (typPat pat))

	fun makeTestSeq (JokPat _, _, rest, mapping) = (rest, mapping)
	  | makeTestSeq (LitPat (_, lit), pos, rest, mapping) =
	    (Test (pos, LitTest lit)::rest, mapping)
	  | makeTestSeq (VarPat (_, id), pos, rest, mapping) =
	    (rest, (pos, id)::mapping)
	  | makeTestSeq (TagPat (info, Lab (_, label), _),
			 pos, rest, mapping) =
	    let
		val n = labelToIndex (#typ info, label)
	    in
		(Test (pos, TagTest (label, n))::rest, mapping)
	    end
	  | makeTestSeq (AppPat (_, TagPat (info, Lab (_, label), isNAry),
				 pat), pos, rest, mapping) =
	    let
		val (posPatList, args) =
		    makeAppArgs (pat, isNAry, LABEL label::pos)
		val typ = #typ info
		val n = labelToIndex (typ, label)
		val conArity = makeConArity (typ, isNAry)
	    in
		List.foldl (fn ((pos, pat), (rest, mapping)) =>
			    makeTestSeq (pat, pos, rest, mapping))
		(Test (pos, TagAppTest (label, n, args, conArity))::rest,
		 mapping) posPatList
	    end
	  | makeTestSeq (ConPat (_, longid, _), pos, rest, mapping) =
	    (Test (pos, ConTest longid)::rest, mapping)
	  | makeTestSeq (AppPat (_, ConPat (info, longid, isNAry), pat),
			 pos, rest, mapping) =
	    let
		val (posPatList, args) =
		    makeAppArgs (pat, isNAry, longidToSelector longid::pos)
		val typ = Type.inArrow (typPat pat, #typ info)
		val conArity = makeConArity (typ, isNAry)
	    in
		List.foldl (fn ((pos, pat), (rest, mapping)) =>
			    makeTestSeq (pat, pos, rest, mapping))
		(Test (pos, ConAppTest (longid, args, conArity))::rest,
		 mapping) posPatList
	    end
	  | makeTestSeq (AppPat (_, RefPat _, pat), pos, rest, mapping) =
	    makeTestSeq (pat, LABEL (Label.fromString "ref")::pos,
			 Test (pos, RefAppTest (typPat pat))::rest, mapping)
	  | makeTestSeq (TupPat (_, pats), pos, rest, mapping) =
	    List.foldli
	    (fn (i, pat, (rest, mapping)) =>
	     makeTestSeq (pat, LABEL (Label.fromInt (i + 1))::pos,
			  rest, mapping))
	    (Test (pos, TupTest (List.map typPat pats))::rest, mapping) pats
	  | makeTestSeq (ProdPat (info, patFields), pos, rest, mapping) =
	    List.foldl (fn (Field (_, Lab (_, label), pat), (rest, mapping)) =>
			makeTestSeq (pat, LABEL label::pos, rest, mapping))
	    (case getRow (#typ info) of
		 (labelTypList, _, true) =>
		     List.foldl (fn ((label, typ), rest) =>
				 let
				     val n = labelToIndex (#typ info, label)
				 in
				     Test (pos, LabTest (label, n, typ))::rest
				 end)
		     rest labelTypList
	       | (labelTypList, LabelSort.Tup _, false) =>
		     Test (pos, TupTest (List.map #2 labelTypList))::rest
	       | (labelTypList, LabelSort.Rec, false) =>
		     Test (pos, RecTest labelTypList)::rest, mapping) patFields
	  | makeTestSeq (VecPat (_, pats), pos, rest, mapping) =
	    List.foldli
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

	(* Debugging *)

	fun posToString' (LABEL l::rest) =
	    Label.toString l ^ "." ^ posToString' rest
	  | posToString' (LONGID _::rest) =
	    "<longid>." ^ posToString' rest
	  | posToString' nil = "<e>"

	fun posToString pos = posToString' (List.rev pos)

	fun indent 0 = ""
	  | indent n = "  " ^ indent (n - 1)

	fun litToString (WordLit w) = LargeWord.toString w
	  | litToString (IntLit i) = LargeInt.toString i
	  | litToString (CharLit c) = "#\"" ^ WideChar.toString c ^ "\""
	  | litToString (StringLit s) = "\"" ^ s ^ "\""
	  | litToString (RealLit s) = s

	fun testToString (LitTest lit) = "lit " ^ litToString lit
	  | testToString (TagTest (label, n) |
			  TagAppTest (label, n, O.OneArg _, _)) =
	    "tag " ^ Label.toString label ^ "/" ^ Int.toString n
	  | testToString (TagAppTest (label, n, O.TupArgs typs, _)) =
	    "tag " ^ Label.toString label ^ "/" ^ Int.toString n ^
	    " tup " ^ Int.toString (List.length typs)
	  | testToString (TagAppTest (label, n, O.RecArgs labelTypList, _)) =
	    "tag " ^ Label.toString label ^ "/" ^ Int.toString n ^ " rec"
	  | testToString (ConTest _ | ConAppTest (_, O.OneArg _, _)) = "con"
	  | testToString (ConAppTest (_, O.TupArgs typs, _)) =
	    "con tup " ^ Int.toString (List.length typs)
	  | testToString (ConAppTest (_, O.RecArgs labelTypList, _)) =
	    "con rec"
	  | testToString (RefAppTest _) = "ref"
	  | testToString (TupTest typs) =
	    "tup " ^ Int.toString (List.length typs)
	  | testToString (RecTest labelTyplist) = "rec"
	  | testToString (LabTest (label, n, _)) =
	    "lab " ^ Label.toString label ^ "/" ^ Int.toString n
	  | testToString (VecTest typs) =
	    "vec " ^ Int.toString (List.length typs)
	  | testToString (GuardTest (_, _)) = "guard"
	  | testToString (DecTest (_, decs)) =
	    "dec " ^ Int.toString (List.length decs)

	fun graphToString (Node (pos, test, ref thenGraph, ref elseGraph, _),
			   level) =
	    indent level ^
	    posToString pos ^ ": " ^
	    testToString test ^ "\n" ^
	    graphToString (thenGraph, level + 1) ^
	    graphToString (elseGraph, level + 1)
	  | graphToString (Leaf (body, _), level) =
	    indent level ^ "leaf " ^
	    (Source.regionToString (#region (O.infoStm (List.hd body)))) ^ "\n"
	  | graphToString (Default, level) = indent level ^ "default\n"

	fun mappingToString' ((pos, _)::mapping) =
	    " " ^ posToString pos ^ mappingToString' mapping
	  | mappingToString' nil = ""

	fun mappingToString mapping =
	    "dom(mapping) =" ^ mappingToString' mapping ^ "\n"

	fun testSeqToString' (Test (pos, test)::rest) =
	    posToString pos ^ ": " ^ testToString test ^ "\n" ^
	    testSeqToString' rest
	  | testSeqToString' (Neg testSeq::rest) =
	    "<neg>\n" ^ testSeqToString' testSeq ^ "</neg>\n" ^
	    testSeqToString' rest
	  | testSeqToString' (Alt testSeqs::rest) =
	    List.foldr (fn (testSeq, s) =>
			"<alt>\n" ^ testSeqToString' testSeq ^ s)
	    "</alt>\n" testSeqs ^
	    testSeqToString' rest
	  | testSeqToString' nil = ""

	fun testSeqToString testSeq =
	    "<seq>\n" ^ testSeqToString' testSeq ^ "</seq>\n"

	(* Construction of Backtracking Test Trees *)

	fun argsEq (O.OneArg _, O.OneArg _) = true
	  | argsEq (O.TupArgs _, O.TupArgs _) = true
	  | argsEq (O.RecArgs _, O.RecArgs _) = true
	  | argsEq (_, _) = false

	fun testEq (LitTest lit1, LitTest lit2) = lit1 = lit2
	  | testEq (TagTest (label1, _), TagTest (label2, _)) = label1 = label2
	  | testEq (TagAppTest (label1, _, args1, _),
		    TagAppTest (label2, _, args2, _)) =
	    label1 = label2 andalso argsEq (args1, args2)
	  | testEq (ConTest longid1, ConTest longid2) =
	    longidToSelector longid1 = longidToSelector longid2
	  | testEq (ConAppTest (longid1, args1, _),
		    ConAppTest (longid2, args2, _)) =
	    longidToSelector longid1 = longidToSelector longid2 andalso
	    argsEq (args1, args2)
	  | testEq (TupTest _, TupTest _) = true
	  | testEq (RecTest _, RecTest _) = true
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
	    fun mergeIntoTree (Test (pos, test)::testSeqRest,
			       thenTree, elseTree) =
		(case findTest (elseTree, pos, test) of
		     SOME (treeRef as ref tree) =>
			 let
			     val newTree =
				 mergeIntoTree (testSeqRest, thenTree, tree)
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
		(*--** this may create duplicate code in some cases. *)
		(* This could be removed by reconstructing the whole graph *)
		(* using hash-consing. *)
		List.foldr
		(fn (testSeq, elseTree) =>
		 mergeIntoTree (testSeq @ testSeqRest, thenTree, elseTree))
		elseTree testSeqs
	      | mergeIntoTree (nil, thenTree, _) = thenTree
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

	fun buildGraph (matches, elseBody) =
	    let
		val (graph, consequents) =
		    List.foldr (fn ((region, pat, thenBody),
				    (elseTree, consequents)) =>
				let
				    val pat' = separateAlt pat
				    val (testSeq, _) =
					makeTestSeq (pat', nil, nil, nil)
				    val r = ref NONE
				    val leaf = Leaf (thenBody, r)
				in
				    (mergeIntoTree (List.rev testSeq,
						    leaf, elseTree),
				     (region, r)::consequents)
				end) (Default, nil) matches
		val elseGraph = Leaf (elseBody, ref NONE)
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

	    fun deconstructs (JokPat _) = false
	      | deconstructs (LitPat _) = raise NotNAry
	      | deconstructs (VarPat (_, _)) = raise BindsAll
	      | deconstructs (TagPat (_, _, _)) = raise NotNAry
	      | deconstructs (ConPat (_, _, _)) = raise NotNAry
	      | deconstructs (RefPat _) = raise NotNAry
	      | deconstructs (TupPat (_, _)) = true
	      | deconstructs (ProdPat (_, _)) = true
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
		    val id = freshId (id_info info)
		in
		    (O.OneArg id, graph, [(nil, id)], consequents)
		end
	      | process (TUP typs, Node (nil, TupTest _, ref graph, _, _),
			 consequents, _) =
		let
		    val ids =
			List.map (fn _ => freshId {region = Source.nowhere})
			typs
		    val labelIdList =
			List.mapi (fn (i, id) =>
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
			List.map (fn (label, _) =>
				  (label, freshId {region = Source.nowhere}))
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
