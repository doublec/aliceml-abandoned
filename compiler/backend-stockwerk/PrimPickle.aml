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

structure PrimPickle :> PRIM_PICKLE =
    struct
	open LargeInt

	type id = int

	type outstream = BinIO.outstream * id ref

	fun inc (_, r as ref id) = (r := id + 1; id)

	datatype label =
	    TAG of int
	  | CLOSURE
	  | ARRAY
	  | ARRAY_ZERO
	  | CELL
	  | CONSTRUCTOR
	  | CON_VAL
	  | GLOBAL_STAMP
	  | TUPLE
	  | VECTOR
	  | VECTOR_ZERO

	type size = Int.int

	val POSINT  = 0w0: Word8.word
	val NEGINT  = 0w1: Word8.word
	val CHUNK   = 0w2: Word8.word
	val BLOCK   = 0w3: Word8.word
	val REF     = 0w4: Word8.word
	val HANDLER = 0w5: Word8.word

	fun outputByte ((outstream, _), w) = BinIO.output1 (outstream, w)

	fun outputUInt (q, i) =
	    if i >= 0x80 then
		(outputByte (q, Word8.fromLargeInt (i mod 0x80 + 0x80));
		 outputUInt (q, i div 0x80))
	    else if i >= 0 then outputByte (q, Word8.fromLargeInt i)
	    else raise Crash.Crash "PrimPickle.outputUInt"

	val maxDataLabel = 0xFEC: int

	fun outputLabel (q, TAG i) = outputUInt (q, i)
	  | outputLabel (q, CLOSURE) = outputUInt (q, 0xFF7)
	  | outputLabel (q, ARRAY) = outputUInt (q, maxDataLabel - 8)
	  | outputLabel (q, ARRAY_ZERO) = outputUInt (q, maxDataLabel - 7)
	  | outputLabel (q, CELL) = outputUInt (q, maxDataLabel - 6)
	  | outputLabel (q, CONSTRUCTOR) = outputUInt (q, maxDataLabel - 5)
	  | outputLabel (q, CON_VAL) = outputUInt (q, maxDataLabel - 4)
	  | outputLabel (q, GLOBAL_STAMP) = outputUInt (q, maxDataLabel - 3)
	  | outputLabel (q, TUPLE) = outputUInt (q, maxDataLabel - 2)
	  | outputLabel (q, VECTOR) = outputUInt (q, maxDataLabel - 1)
	  | outputLabel (q, VECTOR_ZERO) = outputUInt (q, maxDataLabel)

	fun outputCString (q, s) =
	    (List.map (fn c => outputByte (q, Word8.fromInt (Char.ord c)))
	     (String.explode s);
	     outputByte (q, 0w0))

	fun openOut name = (BinIO.openOut name, ref (0: int))

	fun outputInt (q, i) =
	    if i >= 0 then (outputByte (q, POSINT); outputUInt (q, i))
	    else (outputByte (q, NEGINT); outputUInt (q, ~(i + 1)))

	fun outputChunk (q, bytes) =
	    (outputByte (q, CHUNK);
	     outputUInt (q, fromInt (Vector.length bytes));
	     Vector.app (fn b => outputByte (q, b)) bytes; inc q)

	fun outputBlock (q, label, size) =
	    (outputByte (q, BLOCK); outputLabel (q, label);
	     outputUInt (q, fromInt size); inc q)

	fun outputHandler (q, name, label, size) =
	    (outputByte (q, HANDLER); outputCString (q, name);
	     outputLabel (q, label); outputUInt (q, fromInt size); inc q)

	fun outputReference (q, id) = (outputByte (q, REF); outputUInt (q, id))

	fun closeOut (outstream, _) = BinIO.closeOut outstream
    end
