(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* MOZART PICKLE FORMAT (Extract)
 *
 * pickle ::= sysletheader sysletheader sysletheader crc version term eof
 * sysletheader ::= 0x02
 * crc ::= <byte>*4 // LSB first; computed over the rest of the pickle
 * version ::= string // "3#2" or "3#3"
 * string ::= number <byte>*number
 * number ::=
 *    <byte with bit 7 set>* <byte with bit 7 reset>
 *    // starting with lowest bits
 * eof ::= 0x2A
 *
 * term ::= smallInt | bigInt | float
 *       |  atom | uniqueName
 *       |  tuple | list
 *       |  extension | cell
 *       |  ref
 *
 * smallInt ::= 0x01 number // [~0x8000000 ... 0x7FFFFFF]
 * bigInt ::= 0x02 string // decimal representation; unary minus is `~'
 * float ::= 0x03 number number // starting with LSW
 * atom ::= 0x04 label string
 * uniqueName ::= 0x06 label string
 * tuple ::= 0x08 label number term term*number // subtrees in reverse order
 * list ::= 0x09 label term (list | 0x01 term)
 *
 * extension ::= 0x24 label (bytestring | word)
 * bytestring ::= 0x03 string
 * word ::= 0x08 number number // width, value
 *
 * cell ::= 0x2E label term
 *
 * label ::= number
 * ref ::= 0x0A number
 *)

import structure Reflect           from "../../lib/system/Reflect"
import structure Crash             from "../infrastructure/Crash"
import structure StringMap         from "../infrastructure/StringMap"
import structure Crc               from "Crc"
import signature PICKLE_OUT_STREAM from "PICKLE_OUT_STREAM-sig"

structure PickleOutStream :> PICKLE_OUT_STREAM =
    struct
	open LargeWord
	open Word8Array

	infix << ~>>

	val versionString = "3#" ^ (* line wrap, otherwise m4 will not work! *)
			    Int.toString PICKLE_VERSION

	val SYSLETHEADER  = 0wx02: Word8.word

	(* Term DIFs *)
	val SMALLINT      = 0wx01: Word8.word
	val BIGINT        = 0wx02: Word8.word
	val FLOAT         = 0wx03: Word8.word
	val ATOM          = 0wx04: Word8.word
	val UNIQUENAME    = 0wx06: Word8.word
	val TUPLE         = 0wx08: Word8.word
	val LIST          = 0wx09: Word8.word
	val REF           = 0wx0A: Word8.word
	val EXTENSION     = 0wx24: Word8.word
	val EOF           = 0wx2A: Word8.word
	val CELL          = 0wx2E: Word8.word

	(* Extension IDs *)
	val EX_BYTESTRING = 0wx03: Word8.word
	val EX_WORD       = 0wx08: Word8.word

	val initialSize = 256

	type label = word

	type outstream =
	     {array: array ref,
	      index: int ref,
	      label: label ref,
	      crc: Crc.t ref,
	      count: int ref,
	      atoms: label StringMap.t}

	fun outputByte' ({array, index, label, crc, count, atoms}, b) =
	    (if !index = length (!array) then
		 let
		     val old = !array
		     val newSize = Int.div (Int.* (length (!array), 3), 2)
		 in
		     array := Word8Array.array (newSize, 0w0);
		     copy {src = old, si = 0, len = NONE, dst = !array, di = 0}
		 end
	     else ();
	     update (!array, !index, b);
	     index := Int.+ (!index, 1))

	fun outputByte (q as {array, index, label, crc, count, atoms}, b) =
	    (crc := Crc.update (!crc, b);
	     outputByte' (q, b))

	fun inc ({array, index, label, crc, count, atoms}, i) =
	    count := Int.+ (!count, i)

	fun inc1 q = inc (q, 1)

	fun dec1 {array, index, label, crc, count, atoms} =
	    if !count = 0 then
		raise Crash.Crash "PickleOutStream.dec1"
	    else count := Int.- (!count, 1)

	fun newLabel {array, index, label, crc, count, atoms} =
	    let
		val n = !label
	    in
		label := n + 0w1; n
	    end

	fun marshalNumber (q, i) =
	    if i >= 0wx80 then
		(outputByte (q, Word8.fromLarge (i mod 0wx80 + 0wx80));
		 marshalNumber (q, i div 0wx80))
	    else outputByte (q, Word8.fromLarge i)

	fun outputShared (q, label) =
	    (dec1 q; outputByte (q, REF); marshalNumber (q, label))

	fun outputLargeWord (q, i) =
	    (dec1 q;
	     if i > 0wx7FFFFFF orelse i < fromInt ~0x8000000 then
		 marshalSmallInt (q, i)
	     else marshalBigInt (q, i))
	and marshalSmallInt (q, i) =
	    (outputByte (q, SMALLINT); marshalNumber (q, i))
	and marshalBigInt (q, i) =
	    marshalString (q, LargeInt.toString (toLargeInt i))
	and marshalString (q, s) =
	    (marshalNumber (q, fromInt (String.size s));
	     CharVector.app
	     (fn c => outputByte (q, Word8.fromInt (Char.ord c))) s)

	fun outputInt (q, i) = outputLargeWord (q, fromInt i)

	fun outputLargeInt (q, i) = outputLargeWord (q, fromLargeInt i)

	fun outputLargeReal (q, r) =
	    let
		val vec = Reflect.realToVector r
		fun f i = Word8.toLarge (Word8Vector.sub (vec, i))
		val i1 = f 7 + (f 6 << 0w8) + (f 5 << 0w16) + (f 4 << 0w24)
		val i2 = f 3 + (f 2 << 0w8) + (f 1 << 0w16) + (f 0 << 0w24)
	    in
		dec1 q; outputByte (q, FLOAT);
		marshalNumber (q, i1); marshalNumber (q, i2)
	    end

	fun outputAtom (q, s) =
	    case StringMap.lookup (#atoms q, s) of
		SOME label => (outputShared (q, label); label)
	      | NONE =>
		    let
			val label = newLabel q
		    in
			dec1 q; outputByte (q, ATOM);
			marshalNumber (q, label); marshalString (q, s);
			StringMap.insert (#atoms q, s, label); label
		    end

	fun outputBool (q, true) = marshalUniqueName (q, "true")
	  | outputBool (q, false) = marshalUniqueName (q, "false")
	and outputUnit q = marshalUniqueName (q, "unit")
	and marshalUniqueName (q, s) =
	    let
		val label = newLabel q
	    in
		dec1 q; outputByte (q, UNIQUENAME); marshalNumber (q, label);
		marshalString (q, s); label
	    end

	fun outputTuple (q, lab, width) =
	    let
		val label = newLabel q
	    in
		dec1 q; outputByte (q, TUPLE); marshalNumber (q, label);
		marshalNumber (q, fromInt width); inc1 q; outputAtom (q, lab);
		inc (q, width); label
	    end

	fun outputList _ (q, nil) = outputAtom (q, "nil")
	  | outputList outputX (q, x::xs) =
	    let
		val label = newLabel q
	    in
		dec1 q; outputByte (q, LIST); marshalNumber (q, label);
		inc1 q; outputX (q, x); marshalList (q, xs, outputX); label
	    end
	and marshalList (q, x::xs, outputX) =
	    (outputByte (q, LIST); marshalNumber (q, newLabel q);
	     inc1 q; outputX (q, x); marshalList (q, xs, outputX))
	  | marshalList (q, nil, _) = (inc1 q; outputAtom (q, "nil"))

	fun outputString (q, s) =
	    let
		val label = newLabel q
	    in
		dec1 q; outputByte (q, EXTENSION); marshalNumber (q, label);
		outputByte (q, EX_BYTESTRING); marshalString (q, s); label
	    end

	fun outputWord (q, i, w) =
	    let
		val label = newLabel q
		val n = Word.fromInt (Int.- (wordSize, i))
	    in
		dec1 q; outputByte (q, EXTENSION); marshalNumber (q, label);
		outputByte (q, EX_WORD); marshalNumber (q, fromInt i);
		marshalNumber (q, (w << n) ~>> n); label
	    end

	fun outputRef outputX (q, ref x) =
	    let
		val label = newLabel q
	    in
		dec1 q; outputByte (q, CELL); marshalNumber (q, label);
		inc1 q; outputX (q, x); label
	    end

	fun openOut (): outstream =
	    let
		val q =
		    {array = ref (array (initialSize, 0w0)),
		     index = ref 0,
		     label = ref 0w1,
		     crc = ref Crc.initial,
		     count = ref 1,
		     atoms = StringMap.map ()}
	    in
		outputByte' (q, SYSLETHEADER); outputByte' (q, SYSLETHEADER);
		outputByte' (q, SYSLETHEADER); outputByte' (q, 0w0);
		outputByte' (q, 0w0); outputByte' (q, 0w0);
		outputByte' (q, 0w0); marshalString (q, versionString);
		q
	    end

	fun closeOut (q as {array, index, label, crc, count, atoms}) =
	    let
		val _ = outputByte (q, EOF)
		val bytes = Crc.toBytes (!crc)
	    in
		if !count = 0 then ()
		else raise Crash.Crash "PickleOutStream.closeOut";
		update (!array, 3, Word8Vector.sub (bytes, 0));
		update (!array, 4, Word8Vector.sub (bytes, 1));
		update (!array, 5, Word8Vector.sub (bytes, 2));
		update (!array, 6, Word8Vector.sub (bytes, 3));
		extract (!array, 0, SOME (!index))
	    end
    end
