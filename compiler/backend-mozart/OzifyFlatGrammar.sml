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

structure OzifyFlatGrammar :> CODE where type t = string * FlatGrammar.t =
    struct
	open FlatGrammar

	type t = string * FlatGrammar.t

	local
	    (*--** remove global state *)
	    val visited = StampSet.new ()
	in
	    fun init () = StampSet.deleteAll visited

	    fun visit stamp =
		not (StampSet.member (visited, stamp)) before
		StampSet.insert (visited, stamp)
	end

	val output = TextIO.output
	val output1 = TextIO.output1

	fun l q = output1 (q, #"(")
	fun m q = output1 (q, #" ")
	fun h q = output1 (q, #"#")
	fun r q = output1 (q, #")")
	fun f (q, s) = (output (q, s); l q)

	fun outputBool (q, b) = output (q, Bool.toString b)
	fun outputInt (q, n) = output (q, Int.toString n)
	fun outputLargeInt (q, n) = output (q, LargeInt.toString n)
	fun outputLargeWord (q, w) = outputLargeInt (q, LargeWord.toLargeInt w)
	fun outputLargeReal (q, x) = output (q, (*LargeReal.toString*) x)
	fun outputAtom (q, s) =
	    (output1 (q, #"'");
	     output (q, String.toCString s);
	     output1(q, #"'"))

	fun outputStamp (q, stamp) = output (q, Stamp.toString stamp)

	fun outputOption _ (q, NONE) =
	    (f (q, "none"); r q)
	  | outputOption outputX (q, SOME x) =
	    (f (q, "some"); outputX (q, x); r q)

	fun appTail f (x::xr) = (f (x, xr); appTail f xr)
	  | appTail _ nil = ()

	fun outputList _ (q, nil) = output(q, "nil")
	  | outputList outputX (q, xs) =
	    (output1 (q, #"[");
	     appTail (fn (x, xr) =>
		      (outputX (q, x); case xr of nil => () | _ =>  m q)) xs;
	     output1 (q, #"]"))

	fun outputVector _ (q, #[]) = output (q, "nil")
	  | outputVector outputX (q, xs) =
	    (output1 (q, #"["); outputX (q, Vector.sub (xs, 0));
	     Vector.appi (fn (_, x) => (m q; outputX (q, x))) (xs, 1, NONE);
	     output1 (q, #"]"))

	fun outputChar (q, c) = output (q, Int.toString (WideChar.ord c))

	fun outputString (q, s) =
	    outputList outputChar (q, String.explode s)

	fun outputPair (outputA, outputB) (q, (a, b)) =
	    (l q; outputA (q, a); h q; outputB (q, b); r q)

	fun outputTriple (outputA, outputB, outputC) (q, (a, b, c)) =
	    (l q; outputA (q, a); h q; outputB (q, b);
	     h q; outputC (q, c); r q)

	fun outputQuadruple (outputA, outputB, outputC, outputD)
	    (q, (a, b, c, d)) =
	    (l q; outputA (q, a); h q; outputB (q, b);
	     h q; outputC (q, c); h q; outputD (q, d); r q)

	local
	    fun outputRegion (q, ((ll, lc), (rl, rc))) =
		(l q; output (q, Int.toString ll); h q;
		 output (q, Int.toString lc); r q; h q;
		 l q; output (q, Int.toString rl); h q;
		 output (q, Int.toString rc); r q)
	in
	    fun outputIdInfo (q, info: id_info) =
		outputRegion (q, #region info)

	    fun outputStmInfo (q, info: stm_info) =
		outputRegion (q, #region info)

	    fun outputExpInfo (q, info: exp_info) =
		outputRegion (q, #region info)
	end

	fun outputLit (q, WordLit w) =
	    (f (q, "wordLit"); outputLargeWord (q, w); r q)
	  | outputLit (q, IntLit n) =
	    (f (q, "intLit"); outputLargeInt (q, n); r q)
	  | outputLit (q, CharLit c) =
	    (f (q, "charLit"); outputChar (q, c); r q)
	  | outputLit (q, StringLit s) =
	    (f (q, "stringLit"); outputString (q, s); r q)
	  | outputLit (q, RealLit x) =
	    (f (q, "realLit"); outputLargeReal (q, x); r q)

	fun outputLabel (q, label) =
	    case Label.toLargeInt label of
		SOME n => outputLargeInt (q, n)
	      | NONE =>
		    case Label.toString label of
			"true" => output (q, "true")
		      | "false" => output (q, "false")
		      | "::" => output (q, "'|'")
		      | s => outputAtom (q, s)

	fun outputId (q, Id (info, stamp, name)) =
	    (f (q, "id"); outputIdInfo (q, info); m q;
	     outputStamp (q, stamp); m q;
	     case name of
		 Name.ExId s => (f (q, "exId"); outputAtom (q, s); r q)
	       | Name.InId => output (q, "inId");
	     r q)

	fun outputIdDef (q, IdDef id) = (f (q, "idDef"); outputId (q, id); r q)
	  | outputIdDef (q, Wildcard) = output (q, "wildcard")

	fun outputFunFlag (q, PrintName s) =
	    (f (q, "printName"); outputAtom (q, s); r q)
	  | outputFunFlag (q, AuxiliaryOf stamp) =
	    (f (q, "auxiliaryOf"); outputStamp (q, stamp); r q)
	  | outputFunFlag (q, IsToplevel) = output (q, "IsToplevel")

	fun outputCon (q, Con id) = (f (q, "con"); outputId (q, id); r q)
	  | outputCon (q, StaticCon stamp) =
	    (f (q, "staticCon"); outputStamp (q, stamp); r q)

	fun outputArgs outputX (q, OneArg id) =
	    (f (q, "oneArg"); outputX (q, id); r q)
	  | outputArgs outputX (q, TupArgs ids) =
	    (f (q, "tupArgs"); outputVector outputX (q, ids); r q)
	  | outputArgs outputX (q, ProdArgs labelIdVec) =
	    (f (q, "prodArgs");
	     outputVector (outputPair (outputLabel, outputX)) (q, labelIdVec);
	     r q)

	fun outputConArgs outputX = outputOption (outputArgs outputX)

	fun outputStm (q, ValDec (info, idDef, exp)) =
	    (f (q, "valDec"); outputStmInfo (q, info); m q;
	     outputIdDef (q, idDef); m q; outputExp (q, exp); r q)
	  | outputStm (q, RecDec (info, idDefExpVec)) =
	    (f (q, "recDec"); outputStmInfo (q, info); m q;
	     outputVector (outputPair (outputIdDef, outputExp))
	     (q, idDefExpVec); r q)
	  | outputStm (q, RefAppDec (info, idDef, id)) =
	    (f (q, "refAppDec"); outputStmInfo (q, info); m q;
	     outputIdDef (q, idDef); m q; outputId (q, id); r q)
	  | outputStm (q, TupDec (info, idDefs, id)) =
	    (f (q, "tupDec"); outputStmInfo (q, info); m q;
	     outputVector outputIdDef (q, idDefs); m q;
	     outputId (q, id); r q)
	  | outputStm (q, ProdDec (info, labelIdDefVec, id)) =
	    (f (q, "prodDec"); outputStmInfo (q, info); m q;
	     outputVector (outputPair (outputLabel, outputIdDef))
	     (q, labelIdDefVec); m q; outputId (q, id); r q)
	  | outputStm (q, HandleStm (info, body1, idDef, body2, body3,
				     stamp)) =
	    (f (q, "handleStm"); outputStmInfo (q, info); m q;
	     outputBody (q, body1); m q; outputIdDef (q, idDef); m q;
	     outputBody (q, body2); m q; outputBody (q, body3); m q;
	     outputStamp (q, stamp); r q)
	  | outputStm (q, EndHandleStm (info, stamp)) =
	    (f (q, "endHandleStm"); outputStmInfo (q, info); m q;
	     outputStamp (q, stamp); r q)
	  | outputStm (q, TestStm (info, id, tests, body)) =
	    (f (q, "testStm"); outputStmInfo (q, info); m q;
	     outputId (q, id); m q; outputTests (q, tests); m q;
	     outputBody (q, body); r q)
	  | outputStm (q, RaiseStm (info, id)) =
	    (f (q, "raiseStm"); outputStmInfo (q, info); m q;
	     outputId (q, id); r q)
	  | outputStm (q, ReraiseStm (info, id)) =
	    (f (q, "reraiseStm"); outputStmInfo (q, info); m q;
	     outputId (q, id); r q)
	  | outputStm (q, SharedStm (info, body, stamp)) =
	    (if visit stamp then
		 (f (q, "sharedStm"); outputStmInfo (q, info); m q;
		  outputBody (q, body); m q)
	     else
		f (q, "refStm");
	     outputStamp (q, stamp); r q)
	  | outputStm (q, ReturnStm (info, exp)) =
	    (f (q, "returnStm"); outputStmInfo (q, info); m q;
	     outputExp (q, exp); r q)
	  | outputStm (q, IndirectStm (_, ref bodyOpt)) =
	    (output (q, "/* indirect */");
	     List.app (fn stm => (m q; outputStm (q, stm))) (valOf bodyOpt))
	  | outputStm (q, ExportStm (info, exp)) =
	    (f (q, "exportStm"); outputStmInfo (q, info); m q;
	     outputExp (q, exp); r q)
	and outputTests (q, LitTests litBodyVec) =
	    (f (q, "litTests");
	     outputVector (outputPair (outputLit, outputBody)) (q, litBodyVec);
	     r q)
	  | outputTests (q, TagTests tagBodyVec) =
	    (f (q, "tagTests");
	     outputVector (outputQuadruple (outputLabel, outputInt,
					    outputConArgs outputIdDef,
					    outputBody)) (q, tagBodyVec); r q)
	  | outputTests (q, ConTests conBodyVec) =
	    (f (q, "conTests");
	     outputVector (outputTriple (outputCon, outputConArgs outputIdDef,
					 outputBody)) (q, conBodyVec); r q)
	  | outputTests (q, VecTests vecBodyVec) =
	    (f (q, "vecTests");
	     outputVector (outputPair (outputVector outputIdDef, outputBody))
	     (q, vecBodyVec); r q)
	and outputExp (q, LitExp (info, lit)) =
	    (f (q, "litExp"); outputExpInfo (q, info); m q;
	     outputLit (q, lit); r q)
	  | outputExp (q, PrimExp (info, name)) =
	    (f (q, "primExp"); outputExpInfo (q, info); m q;
	     outputAtom (q, name); r q)
	  | outputExp (q, NewExp info) =
	    (f (q, "newExp"); outputExpInfo (q, info); r q)
	  | outputExp (q, VarExp (info, id)) =
	    (f (q, "varExp"); outputExpInfo (q, info); m q;
	     outputId (q, id); r q)
	  | outputExp (q, TagExp (info, label, n)) =
	    (f (q, "tagExp"); outputExpInfo (q, info); m q;
	     outputLabel (q, label); m q; outputInt (q, n); r q)
	  | outputExp (q, ConExp (info, con)) =
	    (f (q, "conExp"); outputExpInfo (q, info); m q;
	     outputCon (q, con); r q)
	  | outputExp (q, TupExp (info, ids)) =
	    (f (q, "tupExp"); outputExpInfo (q, info); m q;
	     outputVector outputId (q, ids); r q)
	  | outputExp (q, ProdExp (info, labelIdVec)) =
	    (f (q, "prodExp"); outputExpInfo (q, info); m q;
	     outputVector (outputPair (outputLabel, outputId)) (q, labelIdVec);
	     r q)
	  | outputExp (q, VecExp (info, ids)) =
	    (f (q, "vecExp"); outputExpInfo (q, info); m q;
	     outputVector outputId (q, ids); r q)
	  | outputExp (q, FunExp (info, stamp, flags, args, body)) =
	    (f (q, "funExp"); outputExpInfo (q, info); m q;
	     outputStamp (q, stamp); m q;
	     outputList outputFunFlag (q, flags); m q;
	     outputArgs outputIdDef (q, args); m q; outputBody (q, body); r q)
	  | outputExp (q, PrimAppExp (info, name, ids)) =
	    (f (q, "primAppExp"); outputExpInfo (q, info); m q;
	     outputAtom (q, name); m q; outputVector outputId (q, ids); r q)
	  | outputExp (q, VarAppExp (info, id, args)) =
	    (f (q, "varAppExp"); outputExpInfo (q, info); m q;
	     outputId (q, id); m q; outputArgs outputId (q, args); r q)
	  | outputExp (q, TagAppExp (info, label, n, args)) =
	    (f (q, "tagAppExp"); outputExpInfo (q, info); m q;
	     outputLabel (q, label); m q; outputInt (q, n); m q;
	     outputArgs outputId (q, args); r q)
	  | outputExp (q, ConAppExp (info, con, args)) =
	    (f (q, "conAppExp"); outputExpInfo (q, info); m q;
	     outputCon (q, con); m q; outputArgs outputId (q, args); r q)
	  | outputExp (q, RefAppExp (info, id)) =
	    (f (q, "refAppExp"); outputExpInfo (q, info); m q;
	     outputId (q, id); r q)
	  | outputExp (q, SelAppExp (info, label, n, id)) =
	    (f (q, "selAppExp"); outputExpInfo (q, info); m q;
	     outputLabel (q, label); m q; outputInt (q, n); m q;
	     outputId (q, id); r q)
	  | outputExp (q, FunAppExp (info, id, stamp, args)) =
	    (f (q, "funAppExp"); outputExpInfo (q, info); m q;
	     outputId (q, id); m q; outputStamp (q, stamp); m q;
	     outputArgs outputId (q, args); r q)
	and outputBody (q, stms) = outputList outputStm (q, stms)

	fun externalize (q, (filename, (imports, (stms, _)))) =
	    (init ();
	     outputString (q, filename); h q; l q;
	     outputVector (fn (q, (idDef, _, url)) =>
			   (l q; outputIdDef (q, idDef);
			    h q; output (q, "unit");
			    h q; outputString (q, Url.toString url); r q))
	     (q, imports);
	     h q; l q; outputBody (q, stms); h q;
	     output (q, "unit"); r q; r q)
    end
