(*
 * Authors:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 1999
 *   Leif Kornstaedt, 1999
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

	fun outputStamp (q, stamp) =
	    if stamp = Prebound.valstamp_false then outputAtom (q, "false")
	    else if stamp = Prebound.valstamp_true then outputAtom (q, "true")
	    else if stamp = Prebound.valstamp_nil then outputAtom (q, "nil")
	    else if stamp = Prebound.valstamp_cons then outputAtom (q, "cons")
	    else if stamp = Prebound.valstamp_ref then outputAtom (q, "ref")
	    else if stamp = Prebound.valstamp_match then outputAtom (q, "Match")
	    else if stamp = Prebound.valstamp_bind then outputAtom (q, "Bind")
	    else output (q, Stamp.toString stamp)

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

	fun outputChar (q, c) = output (q, Int.toString (WideChar.ord c))

	fun outputString (q, s) =
	    outputList outputChar (q, String.explode s)

	fun outputPair (outputA, outputB) (q, (a, b)) =
	    (l q; outputA (q, a); h q; outputB (q, b); r q)

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

	fun outputArgs outputX (q, OneArg id) =
	    (f (q, "oneArg"); outputX (q, id); r q)
	  | outputArgs outputX (q, TupArgs ids) =
	    (f (q, "tupArgs"); outputList outputX (q, ids); r q)
	  | outputArgs outputX (q, RowArgs labelIdList) =
	    (f (q, "rowArgs");
	     outputList (outputPair (outputLabel, outputX)) (q, labelIdList);
	     r q)

	fun outputArity (q, Unary) = output (q, "unary")
	  | outputArity (q, TupArity i) =
	    (f (q, "tupArity"); output (q, Int.toString i); r q)
	  | outputArity (q, RowArity labels) =
	    (f (q, "rowArity"); outputList outputLabel (q, labels); r q)

	val outputConArity = outputOption outputArity

	fun outputTest (q, LitTest lit) =
	    (f (q, "litTest"); outputLit (q, lit); r q)
	  | outputTest (q, TagTest (label, n)) =
	    (f (q, "tagTest"); outputLabel (q, label); m q;
	     outputInt (q, n); r q)
	  | outputTest (q, TagAppTest (label, n, args)) =
	    (f (q, "tagAppTest"); outputLabel (q, label); m q;
	     outputInt (q, n); m q; outputArgs outputId (q, args); r q)
	  | outputTest (q, ConTest id) =
	    (f (q, "conTest"); outputId (q, id); r q)
	  | outputTest (q, ConAppTest (id, args)) =
	    (f (q, "conAppTest"); outputId (q, id); m q;
	     outputArgs outputId (q, args); r q)
	  | outputTest (q, StaticConTest stamp) =
	    (f (q, "staticConTest"); outputStamp (q, stamp); r q)
	  | outputTest (q, StaticConAppTest (stamp, args)) =
	    (f (q, "staticConAppTest"); outputStamp (q, stamp); m q;
	     outputArgs outputId (q, args); r q)
	  | outputTest (q, VecTest ids) =
	    (f (q, "vecTest"); outputList outputId (q, ids); r q)

	fun outputFunFlag (q, PrintName s) =
	    (f (q, "printName"); outputAtom (q, s); r q)
	  | outputFunFlag (q, AuxiliaryOf stamp) =
	    (f (q, "auxiliaryOf"); outputStamp (q, stamp); r q)
	  | outputFunFlag (q, IsToplevel) = output (q, "IsToplevel")

	fun outputStm (q, ValDec (info, id, exp)) =
	    (f (q, "valDec"); outputStmInfo (q, info); m q;
	     outputId (q, id); m q; outputExp (q, exp); r q)
	  | outputStm (q, RecDec (info, idExpList)) =
	    (f (q, "recDec"); outputStmInfo (q, info); m q;
	     outputList (outputPair (outputId, outputExp)) (q, idExpList); r q)
	  | outputStm (q, RefAppDec (info, id1, id2)) =
	    (f (q, "refAppDec"); outputStmInfo (q, info); m q;
	     outputId (q, id1); m q; outputId (q, id2); r q)
	  | outputStm (q, TupDec (info, ids, id)) =
	    (f (q, "tupDec"); outputStmInfo (q, info); m q;
	     outputList outputId (q, ids); m q; outputId (q, id); r q)
	  | outputStm (q, RowDec (info, labelIdList, id)) =
	    (f (q, "rowDec"); outputStmInfo (q, info); m q;
	     outputList (outputPair (outputLabel, outputId)) (q, labelIdList);
	     m q; outputId (q, id); r q)
	  | outputStm (q, EvalStm (info, exp)) =
	    (f (q, "evalStm"); outputStmInfo (q, info); m q;
	     outputExp (q, exp); r q)
	  | outputStm (q, HandleStm (info, body1, id, body2, body3, stamp)) =
	    (f (q, "handleStm"); outputStmInfo (q, info); m q;
	     outputBody (q, body1); m q; outputId (q, id); m q;
	     outputBody (q, body2); m q; outputBody (q, body3); m q;
	     outputStamp (q, stamp); r q)
	  | outputStm (q, EndHandleStm (info, stamp)) =
	    (f (q, "endHandleStm"); outputStmInfo (q, info); m q;
	     outputStamp (q, stamp); r q)
	  | outputStm (q, TestStm (info, id, testBodyList, body)) =
	    (f (q, "testStm"); outputStmInfo (q, info); m q;
	     outputId (q, id); m q;
	     outputList (outputPair (outputTest, outputBody))
	     (q, testBodyList); m q; outputBody (q, body); r q)
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
	and outputExp (q, LitExp (info, lit)) =
	    (f (q, "litExp"); outputExpInfo (q, info); m q;
	     outputLit (q, lit); r q)
	  | outputExp (q, PrimExp (info, name)) =
	    (f (q, "primExp"); outputExpInfo (q, info); m q;
	     outputAtom (q, name); r q)
	  | outputExp (q, NewExp (info, conArity)) =
	    (f (q, "newExp"); outputExpInfo (q, info); m q;
	     outputConArity (q, conArity); r q)
	  | outputExp (q, VarExp (info, id)) =
	    (f (q, "varExp"); outputExpInfo (q, info); m q;
	     outputId (q, id); r q)
	  | outputExp (q, TagExp (info, label, n, conArity)) =
	    (f (q, "tagExp"); outputExpInfo (q, info); m q;
	     outputLabel (q, label); m q; outputInt (q, n); m q;
	     outputConArity (q, conArity); r q)
	  | outputExp (q, ConExp (info, id, conArity)) =
	    (f (q, "conExp"); outputExpInfo (q, info); m q;
	     outputId (q, id); m q; outputConArity (q, conArity); r q)
	  | outputExp (q, StaticConExp (info, stamp, conArity)) =
	    (f (q, "staticConExp"); outputExpInfo (q, info); m q;
	     outputStamp (q, stamp); m q; outputConArity (q, conArity); r q)
	  | outputExp (q, RefExp info) =
	    (f (q, "refExp"); outputExpInfo (q, info); r q)
	  | outputExp (q, TupExp (info, ids)) =
	    (f (q, "tupExp"); outputExpInfo (q, info); m q;
	     outputList outputId (q, ids); r q)
	  | outputExp (q, RowExp (info, labelIdList)) =
	    (f (q, "rowExp"); outputExpInfo (q, info); m q;
	     outputList (outputPair (outputLabel, outputId)) (q, labelIdList);
	     r q)
	  | outputExp (q, VecExp (info, ids)) =
	    (f (q, "vecExp"); outputExpInfo (q, info); m q;
	     outputList outputId (q, ids); r q)
	  | outputExp (q, SelExp (info, label, n)) =
	    (f (q, "selExp"); outputExpInfo (q, info); m q;
	     outputLabel (q, label); m q; outputInt (q, n); r q)
	  | outputExp (q, FunExp (info, stamp, flags, args, body)) =
	    (f (q, "funExp"); outputExpInfo (q, info); m q;
	     outputStamp (q, stamp); m q;
	     outputList outputFunFlag (q, flags); m q;
	     outputArgs outputId (q, args); m q; outputBody (q, body); r q)
	  | outputExp (q, PrimAppExp (info, name, ids)) =
	    (f (q, "primAppExp"); outputExpInfo (q, info); m q;
	     outputAtom (q, name); m q; outputList outputId (q, ids); r q)
	  | outputExp (q, VarAppExp (info, id, args)) =
	    (f (q, "varAppExp"); outputExpInfo (q, info); m q;
	     outputId (q, id); m q; outputArgs outputId (q, args); r q)
	  | outputExp (q, TagAppExp (info, label, n, args)) =
	    (f (q, "tagAppExp"); outputExpInfo (q, info); m q;
	     outputLabel (q, label); m q; outputInt (q, n); m q;
	     outputArgs outputId (q, args); r q)
	  | outputExp (q, ConAppExp (info, id, args)) =
	    (f (q, "conAppExp"); outputExpInfo (q, info); m q;
	     outputId (q, id); m q; outputArgs outputId (q, args); r q)
	  | outputExp (q, StaticConAppExp (info, stamp, args)) =
	    (f (q, "staticConAppExp"); outputExpInfo (q, info); m q;
	     outputStamp (q, stamp); m q; outputArgs outputId (q, args); r q)
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

	fun externalize (q, (filename, (importList, (stms, _)))) =
	    (init ();
	     outputString (q, filename); h q; l q;
	     outputList (fn (q, (id, _, url)) =>
			 (l q; outputId (q, id);
			  h q; output (q, "unit");
			  h q; outputString (q, Url.toString url); r q))
	     (q, importList);
	     h q; l q; outputList outputStm (q, stms); h q;
	     output (q, "unit"); r q; r q)
    end
