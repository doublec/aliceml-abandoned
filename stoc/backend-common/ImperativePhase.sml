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

structure ImperativePhase :> IMPERATIVE_PHASE =
    struct
	structure I = SimplifiedGrammar
	structure O = ImperativeGrammar

	open I
	open Prebound

	val id_false = Id(Source.nowhere, stamp_false, ExId "false")
	val id_true = Id(Source.nowhere, stamp_true,  ExId "true")
	val id_Match = Id(Source.nowhere, stamp_Match, ExId "Match")

	(* State *)

	local
	    val count = ref 0
	in
	    fun gen () =
		let
		    val n = !count + 1
		in
		    count := n; n
		end
	end

	local
	    structure SharedDone =
		MakeHashImpMap(type t = int
			       fun hash i = i)

	    val state: O.body SharedDone.t = SharedDone.new ()
	in
	    fun enterShared (i, stms) = SharedDone.insert (state, i, stms)

	    fun lookupShared i = valOf (SharedDone.lookup (state, i))
	end

	(* Translation *)

	fun share nil = nil
	  | share (stms as [O.SharedStm (_, _, _)]) = stms
	  | share stms =   (*--** provide better coordinates *)
	    [O.SharedStm (Source.nowhere, stms, ref backendInfoDummy)]

	datatype continuation =
	    Decs of dec list * continuation
	  | Goto of O.body
	  | Share of O.body option ref * continuation
	  | Export of id list

	fun freshId coord = Id (coord, Stamp.new (), InId)

	fun translateLongid (ShortId (_, id)) = (nil, id)
	  | translateLongid (LongId (coord, longid, lab)) =
	    let
		val (stms, id) = translateLongid longid
		val id' = freshId coord
		val stm =
		    O.ValDec (coord, id', O.SelAppExp (coord, lab, id), false)
	    in
		(stms @ [stm], id')
	    end

	fun translateTest (LitTest lit) = (nil, O.LitTest lit)
	  | translateTest (ConTest (longid, idOpt)) =
	    let
		val (stms, id) = translateLongid longid
	    in
		(stms, O.ConTest (id, idOpt))
	    end
	  | translateTest (TupTest ids) = (nil, O.TupTest ids)
	  | translateTest (RecTest stringIdList) =
	    (nil, O.RecTest stringIdList)
	  | translateTest (LabTest (string, id)) =
	    (nil, O.LabTest (string, id))
	  | translateTest (VecTest ids) = (nil, O.VecTest ids)

	fun translateCont (Decs (dec::decr, cont)) =
	    translateDec (dec, Decs (decr, cont))
	  | translateCont (Decs (nil, cont)) = translateCont cont
	  | translateCont (Goto stms) = stms
	  | translateCont (Share (r as ref NONE, cont)) =
	    let
		val stms = share (translateCont cont)
	    in
		r := SOME stms; stms
	    end
	  | translateCont (Share (ref (SOME stms), _)) = stms
	  | translateCont (Export ids) = [O.ExportStm (Source.nowhere, ids)]
	and translateDec (OneDec (coord, id, exp), cont) =
	    translateExp (exp,
			  fn exp' => O.ValDec (coord, id, exp', false), cont)
	  | translateDec (ValDec (_, _, exp), cont) =
	    translateExp (exp,
			  fn _ => Crash.crash "ImperativePhase.translateDec 1",
			  cont)
	  | translateDec (RecDec (coord, idExpList), cont) =
	    let
		exception Result of O.exp
		fun result exp' = raise Result exp'
		val idExpList' =
		    List.map (fn (id, exp) =>
			      (translateExp (exp, result, Goto nil);
			       Crash.crash "ImperativePhase.translateDec 2")
			      handle Result exp' => (id, exp')) idExpList
	    in
		O.RecDec (coord, idExpList', false)::translateCont cont
	    end
	  | translateDec (ConDec (coord, id, hasArgs), cont) =
	    O.ConDec (coord, id, hasArgs, false)::translateCont cont
	and translateExp (LitExp (coord, lit), f, cont) =
	    f (O.LitExp (coord, lit))::translateCont cont
	  | translateExp (VarExp (coord, longid), f, cont) =
	    let
		val (stms, id') = translateLongid longid
	    in
		stms @ f (O.VarExp (coord, id'))::translateCont cont
	    end
	  | translateExp (ConExp (coord, longid, NONE, hasArgs), f, cont) =
	    let
		val (stms, id) = translateLongid longid
	    in
		stms @ f (O.ConExp (coord, id, hasArgs))::translateCont cont
	    end
	  | translateExp (ConExp (coord, longid, SOME longid', _), f, cont) =
	    let
		val (stms1, id) = translateLongid longid
		val (stms2, id') = translateLongid longid'
	    in
		stms1 @ stms2 @
		f (O.ConAppExp (coord, id, id'))::translateCont cont
	    end
	  | translateExp (TupExp (coord, longids), f, cont) =
	    let
		val (stms, ids) =
		    List.foldr (fn (longid, (stms, ids)) =>
				let
				    val (stms', id) = translateLongid longid
				in
				    (stms' @ stms, id::ids)
				end) (nil, nil) longids
	    in
		stms @ f (O.TupExp (coord, ids))::translateCont cont
	    end
	  | translateExp (RecExp (coord, labLongidList), f, cont) =
	    let
		val (stms, labIdList) =
		    List.foldr (fn ((lab, longid), (stms, labIdList)) =>
				let
				    val (stms', id) = translateLongid longid
				in
				    (stms' @ stms, (lab, id)::labIdList)
				end) (nil, nil) labLongidList
	    in
		stms @ (f (O.RecExp (coord, labIdList))::translateCont cont)
	    end
	  | translateExp (SelExp (coord, lab, NONE), f, cont) =
	    f (O.SelExp (coord, lab))::translateCont cont
	  | translateExp (SelExp (coord, lab, SOME exp), f, cont) =
	    let
		val coord' = coordOf exp
		val id = freshId coord'
		val stms =
		    f (O.SelAppExp (coord, lab, id))::translateCont cont
	    in
		translateDec (OneDec (coord', id, exp), Goto stms)
	    end
	  | translateExp (VecExp (coord, longids), f, cont) =
	    let
		val (stms, ids) =
		    List.foldr (fn (longid, (stms, ids)) =>
				let
				    val (stms', id) = translateLongid longid
				in
				    (stms' @ stms, id::ids)
				end) (nil, nil) longids
	    in
		stms @ f (O.VecExp (coord, ids))::translateCont cont
	    end
	  | translateExp (FunExp (coord, string, argsExpList), f, cont) =
	    let
		fun translateClause (args, exp) =
		    (args, translateExp (exp,
					 fn exp' =>
					 O.ReturnStm (coordOf exp, exp'),
					 Goto nil))
		val argsExpList' = List.map translateClause argsExpList
	    in
		f (O.FunExp (coord, string, argsExpList'))::translateCont cont
	    end
	  | translateExp (AppExp (coord, longid, exp, _), f, cont) =
	    let
		val (stms, id1) = translateLongid longid
		val coord' = coordOf exp
		val id2 = freshId coord'
		val stms' =
		    f (O.AppExp (coord, id1, O.OneArg id2))::translateCont cont
	    in
		stms @ translateDec (OneDec (coord', id2, exp), Goto stms')
	    end
	  | translateExp (AdjExp (coord, exp1, exp2), f, cont) =
	    let
		val coord1 = coordOf exp1
		val id1 = freshId coord1
		val coord2 = coordOf exp2
		val id2 = freshId coord2
		val stms1 = f (O.AdjExp (coord, id1, id2))::translateCont cont
		val stms2 =
		    translateDec (OneDec (coord2, id2, exp2), Goto stms1)
	    in
		translateDec (OneDec (coord1, id1, exp1), Goto stms2)
	    end
	  | translateExp (WhileExp (coord, exp1, exp2), f, cont) =
	    let
		val r = ref NONE
		val cont' = Goto [O.IndirectStm (coord, r)]
		fun eval exp' = O.EvalStm (coordOf exp2, exp')
		val coord' = coordOf exp1
		val id = freshId coord'
		val trueBody = translateExp (exp2, eval, cont')
		val falseBody = translateExp (TupExp (coord, nil), f, cont)
		val errorBody = [O.RaiseStm (coord', id_Match)]
		val stms1 =
		    [O.TestStm (coord', id, O.ConTest (id_true, NONE),
				trueBody,
				[O.TestStm (coord', id,
					    O.ConTest (id_false, NONE),
					    falseBody, errorBody)])]
		val stms2 =
		    translateDec (OneDec (coord', id, exp1), Goto stms1)
		val stms = share stms2
	    in
		r := SOME stms; stms
	    end
	  | translateExp (SeqExp (_, exps), f, cont) =
	    let
		val isLast = ref true
		fun translate (exp, stms) =
		    if !isLast then
			(case stms of
			     nil => ()
			   | _ => Crash.crash "ImperativePhase.translateExp";
			 isLast := false; translateExp (exp, f, cont))
		    else
			translateExp (exp, (fn exp' =>
					    O.EvalStm (coordOf exp, exp')),
				      Goto stms)
	    in
		List.foldr (fn (exp, stms) => translate (exp, stms)) nil exps
	    end
	  | translateExp (TestExp (coord, longid, test, exp1, exp2), f, cont) =
	    let
		val cont' = Share (ref NONE, cont)
		val (stms, id) = translateLongid longid
		val (stms', test') = translateTest test
	    in
		stms @ stms' @ [O.TestStm (coord, id, test',
					   translateExp (exp1, f, cont'),
					   translateExp (exp2, f, cont'))]
	    end
	  | translateExp (RaiseExp (coord, exp), _, _) =
	    let
		val coord' = coordOf exp
		val id = freshId coord'
	    in
		translateDec (OneDec (coord', id, exp),
			      Goto [O.RaiseStm (coord, id)])
	    end
	  | translateExp (HandleExp (coord, exp1, id, exp2), f, cont) =
	    let
		val cont' = Share (ref NONE, cont)
		val coord' = coordOf exp1
		val id' = freshId coord'
		val stms =
		    translateExp (VarExp (coord', ShortId (coord', id')),
				  f, cont')
		val tryBody =
		    translateDec (OneDec (coord', id', exp1),
				  Goto [O.EndHandleStm (coord, stms)])
	    in
		[O.HandleStm (coord, tryBody, id,
			      translateExp (exp2, f, cont'))]
	    end
	  | translateExp (LetExp (coord, decs, exp), f, cont) =
	    let
		val stms = translateExp (exp, f, cont)
	    in
		translateCont (Decs (decs, Goto stms))
	    end
	  | translateExp (SharedExp (coord, exp, r as ref 0), f, cont) =
	    let
		val n = gen ()
		val _ = r := n
		val stms = translateExp (exp, f, cont)
	    in
		enterShared (n, stms); stms
	    end
	  | translateExp (SharedExp (_, _, ref i), _, _) = lookupShared i
	  | translateExp (DecExp (_, _), _, cont) = translateCont cont

	fun translate (decs, ids) = translateCont (Decs (decs, Export ids))
    end
