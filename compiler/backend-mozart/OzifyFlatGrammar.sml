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

structure OzifyImperativeGrammar :> OZIFY_IMPERATIVE_GRAMMAR =
    struct
	structure I = ImperativeGrammar
	open I

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

	val output = TextIO.output
	val output1 = TextIO.output1

	fun f (q, s) = (output (q, s); output1 (q, #"("))
	fun m q = output1 (q, #" ")
	fun r q = output1 (q, #")")

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
	    if stamp = Prebound.stamp_false then outputAtom (q, "false")
	    else if stamp = Prebound.stamp_true then outputAtom (q, "true")
	    else if stamp = Prebound.stamp_nil then outputAtom (q, "nil")
	    else if stamp = Prebound.stamp_cons then outputAtom (q, "cons")
	    else if stamp = Prebound.stamp_ref then outputAtom (q, "ref")
	    else if stamp = Prebound.stamp_Match then outputAtom (q, "Match")
	    else if stamp = Prebound.stamp_Bind then outputAtom (q, "Bind")
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

	fun outputChar (q, c) = output (q, Int.toString (Char.ord c))

	fun outputString (q, s) =
	    outputList outputChar (q, String.explode s)

	fun outputPair (outputA, outputB) (q, (a, b)) =
	    (output1 (q, #"("); outputA (q, a);
	     output1 (q, #"#"); outputB (q, b); output1 (q, #")"))

	fun outputCoord (q, ((ll, lc), (rl, rc))) =
	    (output (q, Int.toString ll); output1 (q, #"#");
	     output (q, Int.toString lc); output1 (q, #"#");
	     output (q, Int.toString rl); output1 (q, #"#");
	     output (q, Int.toString rc))

	fun outputInfo (q, (coord, _)) = outputCoord (q, coord)   (*--** *)

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

	fun outputLab (q, label) =
	    case Label.toInt label of
		SOME i => outputInt (q, i)
	      | NONE => outputAtom (q, Label.toString label)

	fun outputId (q, Id (info, stamp, name)) =
	    (f (q, "id"); outputInfo (q, info); m q;
	     outputStamp (q, stamp); m q;
	     case name of
		 Name.ExId s => (f (q, "exId"); outputAtom (q, s); r q)
	       | Name.InId => output (q, "inId");
	     r q)

	fun outputConArity (q, Nullary) = output (q, "nullary")
	  | outputConArity (q, Unary) = output (q, "unary")
	  | outputConArity (q, Tuple i) =
	    (f (q, "tuple"); output (q, Int.toString i); r q)
	  | outputConArity (q, Record labels) =
	    (f (q, "record"); outputList outputLab (q, labels); r q)

	fun outputTest (q, LitTest lit) =
	    (f (q, "litTest"); outputLit (q, lit); r q)
	  | outputTest (q, ConTest (id, idOpt, conArity)) =
	    (f (q, "conTest"); outputId (q, id); m q;
	     outputOption outputId (q, idOpt); m q;
	     outputConArity (q, conArity); r q)
	  | outputTest (q, RefTest id) =
	    (f (q, "refTest"); outputId (q, id); r q)
	  | outputTest (q, TupTest ids) =
	    (f (q, "tupTest"); outputList outputId (q, ids); r q)
	  | outputTest (q, RecTest labIdList) =
	    (f (q, "recTest");
	     outputList (outputPair (outputLab, outputId)) (q, labIdList); r q)
	  | outputTest (q, LabTest (lab, id)) =
	    (f (q, "labTest"); outputLab (q, lab); m q; outputId (q, id); r q)
	  | outputTest (q, VecTest ids) =
	    (f (q, "vecTest"); outputList outputId (q, ids); r q)

	fun outputFunFlag (q, PrintName s) =
	    (f (q, "printName"); outputAtom (q, s); r q)
	  | outputFunFlag (q, AuxiliaryOf stamp) =
	    (f (q, "auxiliaryOf"); outputStamp (q, stamp); r q)

	fun outputArgs outputX (q, OneArg id) =
	    (f (q, "oneArg"); outputX (q, id); r q)
	  | outputArgs outputX (q, TupArgs ids) =
	    (f (q, "tupArgs"); outputList outputX (q, ids); r q)
	  | outputArgs outputX (q, RecArgs labIdList) =
	    (f (q, "recArgs");
	     outputList (outputPair (outputLab, outputX)) (q, labIdList); r q)

	fun outputStm (q, ValDec (info, id, exp, isToplevel)) =
	    (f (q, "valDec"); outputInfo (q, info); m q;
	     outputId (q, id); m q; outputExp (q, exp); m q;
	     outputBool (q, isToplevel); r q)
	  | outputStm (q, RecDec (info, idExpList, isToplevel)) =
	    (f (q, "recDec"); outputInfo (q, info); m q;
	     outputList (outputPair (outputId, outputExp)) (q, idExpList); m q;
	     outputBool (q, isToplevel); r q)
	  | outputStm (q, EvalStm (info, exp)) =
	    (f (q, "evalStm"); outputInfo (q, info); m q;
	     outputExp (q, exp); r q)
	  | outputStm (q, HandleStm (info, body1, id, body2, body3, shared)) =
	    (shared := gen ();
	     f (q, "handleStm"); outputInfo (q, info); m q;
	     outputBody (q, body1); m q; outputId (q, id); m q;
	     outputBody (q, body2); m q; outputBody (q, body3); m q;
	     outputInt (q, !shared); r q)
	  | outputStm (q, EndHandleStm (info, ref i)) =
	    (f (q, "endHandleStm"); outputInfo (q, info); m q;
	     outputInt (q, i); r q)
	  | outputStm (q, TestStm (info, id, test, body1, body2)) =
	    (f (q, "testStm"); outputInfo (q, info); m q;
	     outputId (q, id); m q; outputTest (q, test); m q;
	     outputBody (q, body1); m q; outputBody (q, body2); r q)
	  | outputStm (q, RaiseStm (info, id)) =
	    (f (q, "raiseStm"); outputInfo (q, info); m q;
	     outputId (q, id); r q)
	  | outputStm (q, ReraiseStm (info, id)) =
	    (f (q, "reraiseStm"); outputInfo (q, info); m q;
	     outputId (q, id); r q)
	  | outputStm (q, SharedStm (info, body, shared)) =
	    (if !shared = 0 then
		 (shared := gen ();
		  f (q, "sharedStm"); outputInfo (q, info); m q;
		  outputBody (q, body); m q)
	     else
		 f (q, "refStm");
	     outputInt (q, !shared); r q)
	  | outputStm (q, ReturnStm (info, exp)) =
	    (f (q, "returnStm"); outputInfo (q, info); m q;
	     outputExp (q, exp); r q)
	  | outputStm (q, IndirectStm (_, ref bodyOpt)) =
	    (output (q, "/* indirect */");
	     List.app (fn stm => (m q; outputStm (q, stm))) (valOf bodyOpt))
	  | outputStm (q, ExportStm (info, exp)) =
	    (f (q, "exportStm"); outputInfo (q, info); m q;
	     outputExp (q, exp); r q)
	and outputExp (q, LitExp (info, lit)) =
	    (f (q, "litExp"); outputInfo (q, info); m q;
	     outputLit (q, lit); r q)
	  | outputExp (q, PrimExp (info, string)) =
	    (f (q, "primExp"); outputInfo (q, info); m q;
	     outputAtom (q, string); r q)
	  | outputExp (q, NewExp (info, stringOpt, conArity)) =
	    (f (q, "newExp"); outputInfo (q, info); m q;
	     outputOption outputAtom (q, stringOpt); m q;
	     outputConArity (q, conArity); r q)
	  | outputExp (q, VarExp (info, id)) =
	    (f (q, "varExp"); outputInfo (q, info); m q;
	     outputId (q, id); r q)
	  | outputExp (q, ConExp (info, id, conArity)) =
	    (f (q, "conExp"); outputInfo (q, info); m q;
	     outputId (q, id); m q; outputConArity (q, conArity); r q)
	  | outputExp (q, RefExp info) =
	    (f (q, "refExp"); outputInfo (q, info); r q)
	  | outputExp (q, TupExp (info, ids)) =
	    (f (q, "tupExp"); outputInfo (q, info); m q;
	     outputList outputId (q, ids); r q)
	  | outputExp (q, RecExp (info, labIdList)) =
	    (f (q, "recExp"); outputInfo (q, info); m q;
	     outputList (outputPair (outputLab, outputId)) (q, labIdList); r q)
	  | outputExp (q, VecExp (info, ids)) =
	    (f (q, "vecExp"); outputInfo (q, info); m q;
	     outputList outputId (q, ids); r q)
	  | outputExp (q, SelExp (info, lab)) =
	    (f (q, "selExp"); outputInfo (q, info); m q;
	     outputLab (q, lab); r q)
	  | outputExp (q, FunExp (info, stamp, flags, args, body)) =
	    (f (q, "funExp"); outputInfo (q, info); m q;
	     outputStamp (q, stamp); m q;
	     outputList outputFunFlag (q, flags); m q;
	     outputArgs outputId (q, args); m q; outputBody (q, body); r q)
	  | outputExp (q, AppExp (info, id, args)) =
	    (f (q, "appExp"); outputInfo (q, info); m q;
	     outputId (q, id); m q; outputArgs outputId (q, args); r q)
	  | outputExp (q, SelAppExp (info, lab, id)) =
	    (f (q, "selAppExp"); outputInfo (q, info); m q;
	     outputLab (q, lab); m q; outputId (q, id); r q)
	  | outputExp (q, ConAppExp (info, id, args, conArity)) =
	    (f (q, "conAppExp"); outputInfo (q, info); m q;
	     outputId (q, id); m q; outputArgs outputId (q, args); m q;
	     outputConArity (q, conArity); r q)
	  | outputExp (q, RefAppExp (info, args)) =
	    (f (q, "refAppExp"); outputInfo (q, info); m q;
	     outputArgs outputId (q, args); r q)
	  | outputExp (q, PrimAppExp (info, string, ids)) =
	    (f (q, "primAppExp"); outputInfo (q, info); m q;
	     outputAtom (q, string); m q; outputList outputId (q, ids); r q)
	  | outputExp (q, AdjExp (info, id1, id2)) =
	    (f (q, "adjExp"); outputInfo (q, info); m q;
	     outputId (q, id1); m q; outputId (q, id2); r q)
	and outputBody (q, stms) = outputList outputStm (q, stms)

	fun outputComponent (q, (importList, (stms, _))) =
	    (output1 (q, #"(");
	     outputList (fn (q, (id, _, url)) =>
			 outputPair (outputId, outputString)
			 (q, (id, Url.toString url))) (q, importList);
	     output1 (q, #"#");
	     outputList outputStm (q, stms); output1 (q, #")"))
    end
