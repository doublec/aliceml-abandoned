(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *   Andreas Rossberg, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure PickleFlatGrammar :> CODE where type t = string * FlatGrammar.t =
    struct
	open FlatGrammar

	type t = string * FlatGrammar.t

	open PickleOutStream

	fun outputUnit' (q, _) = outputUnit q

	fun outputStamp (q, stamp) = outputInt (q, Stamp.hash stamp)

	fun outputUrl (q, url) = outputString (q, Url.toString url)

	(*--** remove global state *)
	val visited: label StampMap.t = StampMap.new ()

	fun outputOption _ (q, NONE) = ignore (outputAtom (q, "NONE"))
	  | outputOption outputX (q, SOME x) =
	    (outputTuple (q, "SOME", 1); ignore (outputX (q, x)))

	fun appTail f (x::xr) = (f (x, xr); appTail f xr)
	  | appTail _ nil = ()

	fun outputVector _ (q, #[]) = ignore (outputAtom (q, "#[]"))
	  | outputVector outputX (q, xs) =
	    (outputTuple (q, "#[]", Vector.length xs);
	     Vector.appr (fn x => ignore (outputX (q, x))) xs)

	fun outputChar (q, c) = outputInt (q, WideChar.ord c)

	fun outputPair (outputA, outputB) (q, (a, b)) =
	    (outputTuple (q, "#", 2); outputB (q, b); outputA (q, a))

	fun outputTriple (outputA, outputB, outputC) (q, (a, b, c)) =
	    (outputTuple (q, "#", 3); outputC (q, c); outputB (q, b);
	     outputA (q, a))

	fun outputQuadruple (outputA, outputB, outputC, outputD)
	    (q, (a, b, c, d)) =
	    (outputTuple (q, "#", 4); outputD (q, d); outputC (q, c);
	     outputB (q, b); outputA (q, a))

	local
	    val outputIntPair = outputPair (outputInt, outputInt)

	    fun outputRegion (q, region) =
		outputPair (outputIntPair, outputIntPair) (q, region)
	in
	    fun outputIdInfo (q, info: id_info) =
		outputRegion (q, #region info)

	    fun outputStmInfo (q, info: stm_info) =
		outputRegion (q, #region info)

	    fun outputExpInfo (q, info: exp_info) =
		outputRegion (q, #region info)
	end

	fun outputLit (q, WordLit w) =
	    (outputTuple (q, "WordLit", 1); ignore (outputWord (q, 31, w)))
	  | outputLit (q, IntLit n) =
	    (outputTuple (q, "IntLit", 1); outputLargeInt (q, n))
	  | outputLit (q, CharLit c) =
	    (outputTuple (q, "CharLit", 1); outputChar (q, c))
	  | outputLit (q, StringLit s) =
	    (outputTuple (q, "StringLit", 1); ignore (outputString (q, s)))
	  | outputLit (q, RealLit r) =
	    (outputTuple (q, "RealLit", 1); outputLargeReal (q, r))

	fun outputLabel (q, label) =
	    case Label.toLargeInt label of
		SOME n => outputLargeInt (q, n)
	      | NONE =>
		    case Label.toString label of
			"true" => ignore (outputBool (q, true))
		      | "false" => ignore (outputBool (q, false))
		      | "::" => ignore (outputAtom (q, "|"))
		      | s => ignore (outputAtom (q, s))

	fun outputId (q, Id (info, stamp, name)) =
	    (outputTuple (q, "Id", 3);
	     case name of
		 Name.ExId s => (outputTuple (q, "ExId", 1); outputAtom (q, s))
	       | Name.InId => outputAtom (q, "InId");
	     outputStamp (q, stamp); outputIdInfo (q, info))

	fun outputIdDef (q, IdDef id) =
	    (outputTuple (q, "IdDef", 1); outputId (q, id))
	  | outputIdDef (q, Wildcard) = ignore (outputAtom (q, "Wildcard"))

	fun outputFunFlag (q, PrintName s) =
	    (outputTuple (q, "PrintName", 1); ignore (outputAtom (q, s)))
	  | outputFunFlag (q, AuxiliaryOf stamp) =
	    (outputTuple (q, "AuxiliaryOf", 1); outputStamp (q, stamp))
	  | outputFunFlag (q, IsToplevel) =
	    ignore (outputAtom (q, "IsToplevel"))

	fun outputCon (q, Con id) =
	    (outputTuple (q, "Con", 1); outputId (q, id))
	  | outputCon (q, StaticCon stamp) =
	    (outputTuple (q, "StaticCon", 1); outputStamp (q, stamp))

	fun outputArgs outputX (q, OneArg id) =
	    (outputTuple (q, "OneArg", 1); ignore (outputX (q, id)))
	  | outputArgs outputX (q, TupArgs ids) =
	    (outputTuple (q, "TupArgs", 1); outputVector outputX (q, ids))
	  | outputArgs outputX (q, ProdArgs labelIdVec) =
	    (outputTuple (q, "ProdArgs", 1);
	     outputVector (outputPair (outputLabel, outputX)) (q, labelIdVec))

	fun outputConArgs outputX = outputOption (outputArgs outputX)

	fun outputProd (q, Tuple n) =
	    (outputTuple (q, "Tuple", 1); outputInt (q, n))
	  | outputProd (q, Product labels) =
	    (outputTuple (q, "Product", 1);
	     outputVector outputLabel (q, labels))

	fun outputStm (q, ValDec (info, idDef, exp)) =
	    (outputTuple (q, "ValDec", 3); outputExp (q, exp);
	     outputIdDef (q, idDef); outputStmInfo (q, info))
	  | outputStm (q, RefAppDec (info, idDef, id)) =
	    (outputTuple (q, "RefAppDec", 3); outputId (q, id);
	     outputIdDef (q, idDef); outputStmInfo (q, info))
	  | outputStm (q, TupDec (info, idDefs, id)) =
	    (outputTuple (q, "TupDec", 3); outputId (q, id);
	     outputVector outputIdDef (q, idDefs); outputStmInfo (q, info))
	  | outputStm (q, ProdDec (info, labelIdDefVec, id)) =
	    (outputTuple (q, "ProdDec", 3); outputId (q, id);
	     outputVector (outputPair (outputLabel, outputIdDef))
	     (q, labelIdDefVec);
	     outputStmInfo (q, info))
	  | outputStm (q, TryStm (info, tryBody, idDef, handleBody)) =
	    (outputTuple (q, "TryStm", 4); outputBody (q, handleBody);
	     outputIdDef (q, idDef); outputBody (q, tryBody);
	     outputStmInfo (q, info))
	  | outputStm (q, EndTryStm (info, body)) =
	    (outputTuple (q, "EndTryStm", 2); outputBody (q, body);
	     outputStmInfo (q, info))
	  | outputStm (q, EndHandleStm (info, body)) =
	    (outputTuple (q, "EndHandleStm", 2); outputBody (q, body);
	     outputStmInfo (q, info))
	  | outputStm (q, TestStm (info, id, tests, body)) =
	    (outputTuple (q, "TestStm", 4); outputBody (q, body);
	     outputTests (q, tests); outputId (q, id); outputStmInfo (q, info))
	  | outputStm (q, RaiseStm (info, id)) =
	    (outputTuple (q, "RaiseStm", 2); outputId (q, id);
	     outputStmInfo (q, info))
	  | outputStm (q, ReraiseStm (info, id)) =
	    (outputTuple (q, "ReraiseStm", 2); outputId (q, id);
	     outputStmInfo (q, info))
	  | outputStm (q, SharedStm (info, body, stamp)) =
	    (case StampMap.lookup (visited, stamp) of
		 SOME label => outputShared (q, label)
	       | NONE =>
		     let
			 val label = outputTuple (q, "SharedStm", 3)
		     in
			 outputStamp (q, stamp); outputBody (q, body);
			 outputStmInfo (q, info);
			 StampMap.insert (visited, stamp, label)
		     end)
	  | outputStm (q, ReturnStm (info, exp)) =
	    (outputTuple (q, "ReturnStm", 2); outputExp (q, exp);
	     outputStmInfo (q, info))
	  | outputStm (q, IndirectStm (info, bodyOptRef)) =
	    (outputTuple (q, "IndirectStm", 2);
	     outputRef (outputOption outputBody) (q, bodyOptRef);
	     outputStmInfo (q, info))
	  | outputStm (q, ExportStm (info, exp)) =
	    (outputTuple (q, "ExportStm", 2); outputExp (q, exp);
	     outputStmInfo (q, info))
	and outputTests (q, LitTests litBodyVec) =
	    (outputTuple (q, "LitTests", 1);
	     outputVector (outputPair (outputLit, outputBody)) (q, litBodyVec))
	  | outputTests (q, TagTests tagBodyVec) =
	    (outputTuple (q, "TagTests", 1);
	     outputVector (outputQuadruple (outputLabel, outputInt,
					    outputConArgs outputIdDef,
					    outputBody)) (q, tagBodyVec))
	  | outputTests (q, ConTests conBodyVec) =
	    (outputTuple (q, "ConTests", 1);
	     outputVector (outputTriple (outputCon, outputConArgs outputIdDef,
					 outputBody)) (q, conBodyVec))
	  | outputTests (q, VecTests vecBodyVec) =
	    (outputTuple (q, "VecTests", 1);
	     outputVector (outputPair (outputVector outputIdDef, outputBody))
	     (q, vecBodyVec))
	and outputExp (q, LitExp (info, lit)) =
	    (outputTuple (q, "LitExp", 2); outputLit (q, lit);
	     outputExpInfo (q, info))
	  | outputExp (q, PrimExp (info, name)) =
	    (outputTuple (q, "PrimExp", 2); outputAtom (q, name);
	     outputExpInfo (q, info))
	  | outputExp (q, NewExp info) =
	    (outputTuple (q, "NewExp", 1); outputExpInfo (q, info))
	  | outputExp (q, VarExp (info, id)) =
	    (outputTuple (q, "VarExp", 2); outputId (q, id);
	     outputExpInfo (q, info))
	  | outputExp (q, TagExp (info, label, n)) =
	    (outputTuple (q, "TagExp", 3); outputInt (q, n);
	     outputLabel (q, label); outputExpInfo (q, info))
	  | outputExp (q, ConExp (info, con)) =
	    (outputTuple (q, "ConExp", 2); outputCon (q, con);
	     outputExpInfo (q, info))
	  | outputExp (q, TupExp (info, ids)) =
	    (outputTuple (q, "TupExp", 2); outputVector outputId (q, ids);
	     outputExpInfo (q, info))
	  | outputExp (q, ProdExp (info, labelIdVec)) =
	    (outputTuple (q, "ProdExp", 2);
	     outputVector (outputPair (outputLabel, outputId)) (q, labelIdVec);
	     outputExpInfo (q, info))
	  | outputExp (q, VecExp (info, ids)) =
	    (outputTuple (q, "VecExp", 2); outputVector outputId (q, ids);
	     outputExpInfo (q, info))
	  | outputExp (q, FunExp (info, stamp, flags, args, body)) =
	    (outputTuple (q, "FunExp", 5); outputBody (q, body);
	     outputArgs outputIdDef (q, args);
	     outputList outputFunFlag (q, flags);
	     outputStamp (q, stamp); outputExpInfo (q, info))
	  | outputExp (q, PrimAppExp (info, name, ids)) =
	    (outputTuple (q, "PrimAppExp", 3); outputVector outputId (q, ids);
	     outputAtom (q, name); outputExpInfo (q, info))
	  | outputExp (q, VarAppExp (info, id, args)) =
	    (outputTuple (q, "VarAppExp", 3); outputArgs outputId (q, args);
	     outputId (q, id); outputExpInfo (q, info))
	  | outputExp (q, TagAppExp (info, label, n, args)) =
	    (outputTuple (q, "TagAppExp", 4); outputArgs outputId (q, args);
	     outputInt (q, n); outputLabel (q, label); outputExpInfo (q, info))
	  | outputExp (q, ConAppExp (info, con, args)) =
	    (outputTuple (q, "ConAppExp", 3); outputArgs outputId (q, args);
	     outputCon (q, con); outputExpInfo (q, info))
	  | outputExp (q, RefAppExp (info, id)) =
	    (outputTuple (q, "RefAppExp", 2); outputId (q, id);
	     outputExpInfo (q, info))
	  | outputExp (q, SelAppExp (info, prod, label, n, id)) =
	    (outputTuple (q, "SelAppExp", 5); outputId (q, id);
	     outputInt (q, n); outputLabel (q, label); outputProd (q, prod);
	     outputExpInfo (q, info))
	  | outputExp (q, FunAppExp (info, id, stamp, args)) =
	    (outputTuple (q, "FunAppExp", 4); outputArgs outputId (q, args);
	     outputStamp (q, stamp); outputId (q, id); outputExpInfo (q, info))
	and outputBody (q, stms) = outputList outputStm (q, stms)

	fun externalize (q, (filename, (imports, (stms, _)))) =
	    let
		val q' = openOut ()
		val filename' = filename ^ ".ozp"
		val outstream = BinIO.openOut filename'
	    in
		StampMap.deleteAll visited;
		outputPair (outputString,
			    outputPair (outputVector
					(outputTriple (outputIdDef,
						       outputUnit',
						       outputUrl)),
					outputPair (outputBody, outputUnit')))
		(q', (filename, (imports, (stms, ()))));
		BinIO.output (outstream, closeOut q');
		BinIO.closeOut outstream;
		TextIO.output (q,  OS.FileSys.fullPath filename')
	    end
    end
