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
	open ImperativeGrammar

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
	fun outputString (q, s) =
	    (output1 (q, #"\"");
	     output (q, String.toCString s);
	     output1 (q, #"\""))
	fun outputAtom (q, s) =
	    (output1 (q, #"'");
	     output (q, String.toCString s);
	     output1(q, #"'"))

	fun outputStamp (q, n) = output (q, Stamp.toString n)

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

	fun outputPair (outputA, outputB) (q, (a, b)) =
	    (output1 (q, #"("); outputA (q, a);
	     output1 (q, #"#"); outputB (q, b); output1 (q, #")"))

	fun outputCoord (q, (l, r)) =
	    (output (q, Int.toString l);
	     output1 (q, #"#");
	     output (q, Int.toString r))

	fun outputLit (q, WordLit w) =
	    (f (q, "wordLit"); outputLargeWord (q, w); r q)
	  | outputLit (q, IntLit n) =
	    (f (q, "intLit"); outputLargeInt (q, n); r q)
	  | outputLit (q, CharLit c) =
	    (f (q, "charLit"); outputString (q, String.str c); r q)
	  | outputLit (q, StringLit s) =
	    (f (q, "stringLit"); outputString (q, s); r q)
	  | outputLit (q, RealLit x) =
	    (f (q, "realLit"); output (q, x); r q)

	fun outputLabString (q, s) =
	    case Int.fromString s of
		NONE => outputAtom (q, s)
	      | SOME n => outputInt (q, n)

	fun outputLab (q, Lab (coord, s)) =
	    (f (q, "lab"); outputCoord (q, coord); m q;
	     outputLabString (q, s); r q)

	fun outputId (q, Id (coord, stamp, name)) =
	    (f (q, "id"); outputCoord (q, coord); m q;
	     if stamp = Prebound.stamp_false then outputAtom (q, "false")
	     else if stamp = Prebound.stamp_true then outputAtom (q, "true")
	     else if stamp = Prebound.stamp_nil then outputAtom (q, "nil")
	     else if stamp = Prebound.stamp_cons then outputAtom (q, "cons")
	     else if stamp = Prebound.stamp_ref then outputAtom (q, "ref")
	     else if stamp = Prebound.stamp_Match then outputAtom (q, "Match")
	     else if stamp = Prebound.stamp_Bind then outputAtom (q, "Bind")
	     else if stamp = Prebound.stamp_eq then outputAtom (q, "eq")
	     else if stamp = Prebound.stamp_assign then outputAtom (q, "assign")
	     else if stamp = Prebound'.stamp_builtin then outputAtom (q, "builtin")
	     else if stamp = Prebound'.stamp_less then outputAtom (q, "<")
	     else if stamp = Prebound'.stamp_plus then outputAtom (q, "+")
	     else if stamp = Prebound'.stamp_times then outputAtom (q, "*")
	     else outputStamp (q, stamp); m q;
	     case name of
		 ExId s => (f (q, "exId"); outputAtom (q, s); r q)
	       | InId => output (q, "inId");
	     r q)

	fun outputTest (q, LitTest lit) =
	    (f (q, "litTest"); outputLit (q, lit); r q)
	  | outputTest (q, ConTest (id, idOpt)) =
	    (f (q, "conTest"); outputId (q, id); m q;
	     outputOption outputId (q, idOpt); r q)
	  | outputTest (q, TupTest ids) =
	    (f (q, "tupTest"); outputList outputId (q, ids); r q)
	  | outputTest (q, RecTest stringIdList) =
	    (f (q, "recTest");
	     outputList (outputPair (outputLabString, outputId))
	     (q, stringIdList); r q)
	  | outputTest (q, LabTest (string, id)) =
	    (f (q, "labTest"); outputLabString (q, string); output1 (q, #"#");
	     outputId (q, id); r q)

	fun outputArgs outputX (q, OneArg id) =
	    (f (q, "oneArg"); outputX (q, id); r q)
	  | outputArgs outputX (q, TupArgs ids) =
	    (f (q, "tupArgs"); outputList outputX (q, ids); r q)
	  | outputArgs outputX (q, RecArgs stringIdList) =
	    (f (q, "recArgs");
	     outputList (outputPair (outputAtom, outputX))
	     (q, stringIdList); r q)

	fun outputStm (q, ValDec (coord, id, exp, isToplevel)) =
	    (f (q, "valDec"); outputCoord (q, coord); m q;
	     outputId (q, id); m q; outputExp (q, exp); m q;
	     outputBool (q, isToplevel); r q)
	  | outputStm (q, RecDec (coord, idsExpList, isToplevel)) =
	    (f (q, "recDec"); outputCoord (q, coord); m q;
	     outputList (outputPair (outputList outputId, outputExp))
	     (q, idsExpList); m q; outputBool (q, isToplevel); r q)
	  | outputStm (q, ConDec (coord, id, hasArgs, isToplevel)) =
	    (f (q, "conDec"); outputCoord (q, coord); m q;
	     outputId (q, id); m q; outputBool (q, hasArgs); m q;
	     outputBool (q, isToplevel); r q)
	  | outputStm (q, EvalStm (coord, exp)) =
	    (f (q, "evalStm"); outputCoord (q, coord); m q;
	     outputExp (q, exp); r q)
	  | outputStm (q, HandleStm (coord, body1, id, body2)) =
	    (f (q, "handleStm"); outputCoord (q, coord); m q;
	     outputBody (q, body1); m q; outputId (q, id); m q;
	     outputBody (q, body2); r q)
	  | outputStm (q, EndHandleStm (coord, body)) =
	    (f (q, "endHandleStm"); outputCoord (q, coord); m q;
	     outputBody (q, body); r q)
	  | outputStm (q, TestStm (coord, id, test, body1, body2)) =
	    (f (q, "testStm"); outputCoord (q, coord); m q;
	     outputId (q, id); m q; outputTest (q, test); m q;
	     outputBody (q, body1); m q; outputBody (q, body2); r q)
	  | outputStm (q, RaiseStm (coord, id)) =
	    (f (q, "raiseStm"); outputCoord (q, coord); m q;
	     outputId (q, id); r q)
	  | outputStm (q, SharedStm (coord, body, shared)) =
	    (if !shared = 0 then
		 (shared := gen ();
		  f (q, "sharedStm"); outputCoord (q, coord); m q;
		  outputBody (q, body); m q)
	     else
		 f (q, "refStm");
	     outputInt (q, !shared); r q)
	  | outputStm (q, ReturnStm (coord, exp)) =
	    (f (q, "returnStm"); outputCoord (q, coord); m q;
	     outputExp (q, exp); r q)
	  | outputStm (q, IndirectStm (_, ref bodyOpt)) =
	    List.app (fn stm => (outputStm (q, stm); output1 (q, #" ")))
	    (valOf bodyOpt)
	  | outputStm (q, ExportStm (coord, ids)) =
	    (f (q, "exportStm"); outputCoord (q, coord); m q;
	     outputList outputId (q, ids); r q)
	and outputExp (q, LitExp (coord, lit)) =
	    (f (q, "litExp"); outputCoord (q, coord); m q;
	     outputLit (q, lit); r q)
	  | outputExp (q, VarExp (coord, id)) =
	    (f (q, "varExp"); outputCoord (q, coord); m q;
	     outputId (q, id); r q)
	  | outputExp (q, TupExp (coord, ids)) =
	    (f (q, "tupExp"); outputCoord (q, coord); m q;
	     outputList outputId (q, ids); r q)
	  | outputExp (q, RecExp (coord, labIdList)) =
	    (f (q, "recExp"); outputCoord (q, coord); m q;
	     outputList (outputPair (outputLab, outputId)) (q, labIdList); r q)
	  | outputExp (q, SelExp (coord, lab)) =
	    (f (q, "selExp"); outputCoord (q, coord); m q;
	     outputLab (q, lab); r q)
	  | outputExp (q, FunExp (coord, string, argsBodyList)) =
	    (f (q, "funExp"); outputCoord (q, coord); m q;
	     outputAtom (q, string); m q;
	     outputList (outputPair (outputArgs outputId, outputBody))
	     (q, argsBodyList); r q)
	  | outputExp (q, AppExp (coord, id, args)) =
	    (f (q, "appExp"); outputCoord (q, coord); m q;
	     outputId (q, id); m q; outputArgs outputId (q, args); r q)
	  | outputExp (q, SelAppExp (coord, lab, id)) =
	    (f (q, "conAppExp"); outputCoord (q, coord); m q;
	     outputLab (q, lab); m q; outputId (q, id); r q)
	  | outputExp (q, ConAppExp (coord, id1, id2)) =
	    (f (q, "conAppExp"); outputCoord (q, coord); m q;
	     outputId (q, id1); m q; outputId (q, id2); r q)
	  | outputExp (q, BuiltinAppExp (coord, string, ids)) =
	    (f (q, "builtinAppExp"); outputCoord (q, coord); m q;
	     outputAtom (q, string); m q; outputList outputId (q, ids); r q)
	  | outputExp (q, AdjExp (coord, id1, id2)) =
	    (f (q, "adjExp"); outputCoord (q, coord); m q;
	     outputId (q, id1); m q; outputId (q, id2); r q)
	and outputBody (q, stms) = outputList outputStm (q, stms)

	val outputProgram = outputList outputStm
    end
