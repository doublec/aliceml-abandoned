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

structure OzifySimplified :> OZIFYSIMPLIFIED =
    struct
	structure Simplified = Simplified
	open Simplified

	local
	    val count = ref 0
	in
	    fun gen () =
		let
		    val n = !count
		in
		    count := n + 1; n
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
	    (outputA (q, a); output1 (q, #"#"); outputB (q, b))

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

	fun outputId (q, Id (coord, stamp, name)) =
	    (f (q, "id"); outputCoord (q, coord); m q;
	     outputInt (q, stamp); m q;
	     case name of
		 ExId s => (f (q, "exId"); outputAtom (q, s); r q)
	       | InId => output (q, "inId");
	     r q)

	fun outputLongid (q, ShortId (coord, id)) =
	    (f (q, "shortId"); outputCoord (q, coord); m q;
	     outputId (q, id); r q)
	  | outputLongid (q, LongId (coord, longid, id)) =
	    (f (q, "longId"); outputCoord (q, coord); m q;
	     outputLongid (q, longid); m q; outputId (q, id); r q)

	fun outputLabString (q, s) =
	    case Int.fromString s of
		NONE => outputAtom (q, s)
	      | SOME n => outputInt (q, n)

	fun outputLab (q, Lab (coord, s)) =
	    (f (q, "lab"); outputCoord (q, coord); m q;
	     outputLabString (q, s); r q)

	fun outputDec (q, OneDec (coord, id, exp)) =
	    (f (q, "oneDec"); outputCoord (q, coord); m q;
	     outputId (q, id); m q; outputExp (q, exp); r q)
	  | outputDec (q, ValDec (coord, ids, exp)) =
	    (f (q, "valDec"); outputCoord (q, coord); m q;
	     outputList outputId (q, ids); m q; outputExp (q, exp); r q)
	  | outputDec (q, RecDec (coord, idExps)) =
	    (f (q, "recDec"); outputCoord (q, coord); m q;
	     outputList (outputPair (outputId, outputExp)) (q, idExps); r q)
	  | outputDec (q, ConDec (coord, id, hasArgs)) =
	    (f (q, "conDec"); outputCoord (q, coord); m q;
	     outputId (q, id); m q; outputBool (q, hasArgs); r q)
	and outputExp (q, LitExp (coord, lit)) =
	    (f (q, "litExp"); outputCoord (q, coord); m q;
	     outputLit (q, lit); r q)
	  | outputExp (q, VarExp (coord, longid)) =
	    (f (q, "varExp"); outputCoord (q, coord); m q;
	     outputLongid (q, longid); r q)
	  | outputExp (q, ConExp (coord, longid, longidOpt)) =
	    (f (q, "conExp"); outputCoord (q, coord); m q;
	     outputLongid (q, longid); m q;
	     outputOption outputLongid (q, longidOpt); r q)
	  | outputExp (q, TupExp (coord, longids)) =
	    (f (q, "tupExp"); outputCoord (q, coord); m q;
	     outputList outputLongid (q, longids); r q)
	  | outputExp (q, RecExp (coord, labLongidList)) =
	    (f (q, "recExp"); outputCoord (q, coord); m q;
	     outputList (outputPair (outputLab, outputLongid))
	     (q, labLongidList); r q)
	  | outputExp (q, SelExp (coord, lab)) =
	    (f (q, "selExp"); outputCoord (q, coord); m q;
	     outputLab (q, lab); r q)
	  | outputExp (q, FunExp (coord, string, id, exp)) =
	    (f (q, "funExp"); outputCoord (q, coord); m q;
	     outputAtom (q, string); m q; outputId (q, id); m q;
	     outputExp (q, exp); r q)
	  | outputExp (q, AppExp (coord, exp1, exp2)) =
	    (f (q, "appExp"); outputCoord (q, coord); m q;
	     outputExp (q, exp1); m q; outputExp (q, exp2); r q)
	  | outputExp (q, AdjExp (coord, exp1, exp2)) =
	    (f (q, "adjExp"); outputCoord (q, coord); m q;
	     outputExp (q, exp1); m q; outputExp (q, exp2); r q)
	  | outputExp (q, WhileExp (coord, exp1, exp2)) =
	    (f (q, "whileExp"); outputCoord (q, coord); m q;
	     outputExp (q, exp1); m q; outputExp (q, exp2); r q)
	  | outputExp (q, SeqExp (coord, exps)) =
	    (f (q, "seqExp"); outputCoord (q, coord); m q;
	     outputList outputExp (q, exps); r q)
	  | outputExp (q, TestExp (coord, longid, test, exp1, exp2)) =
	    (f (q, "testExp"); outputCoord (q, coord); m q;
	     outputLongid (q, longid); m q;
	     outputTest (q, test); m q; outputExp (q, exp1); m q;
	     outputExp (q, exp2); r q)
	  | outputExp (q, RaiseExp (coord, exp)) =
	    (f (q, "raiseExp"); outputCoord (q, coord); m q;
	     outputExp (q, exp); r q)
	  | outputExp (q, HandleExp (coord, exp1, id, exp2)) =
	    (f (q, "handleExp"); outputCoord (q, coord); m q;
	     outputExp (q, exp1); m q; outputId (q, id); m q;
	     outputExp (q, exp2); r q)
	  | outputExp (q, LetExp (coord, decs, exp)) =
	    (f (q, "letExp"); outputCoord (q, coord); m q;
	     outputList outputDec (q, decs); m q; outputExp (q, exp); r q)
	  | outputExp (q, SharedExp (coord, exp, shared)) =
	    (if !shared = 0 then
		 (shared := gen ();
		  f (q, "sharedExp"); outputCoord (q, coord); m q;
		  outputExp (q, exp))
	     else
		 f (q, "refExp");
	     outputInt (q, !shared); r q)
	  | outputExp (q, DecExp (coord, ids)) =
	    (f (q, "decExp"); outputCoord (q, coord); m q;
	     outputList outputId (q, ids); r q)
	and outputTest (q, LitTest lit) =
	    (f (q, "litTest"); outputLit (q, lit); r q)
	  | outputTest (q, NameTest longid) =
	    (f (q, "nameTest"); outputLongid (q, longid); r q)
	  | outputTest (q, ConTest (longid, id)) =
	    (f (q, "nameTest"); outputLongid (q, longid); m q;
	     outputId (q, id); r q)
	  | outputTest (q, RecTest stringIdList) =
	    (f (q, "recTest");
	     outputList (outputPair (outputLabString, outputId))
	     (q, stringIdList); r q)
	  | outputTest (q, LabTest (string, id)) =
	    (f (q, "labTest"); outputLabString (q, string); output1 (q, #"#");
	     outputId (q, id); r q)
    end
