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

signature PICKLE_OUT_STREAM =
    sig
	type outstream
	type label

	val openOut: unit -> outstream
	val closeOut: outstream -> Word8Vector.vector

	val outputInt: outstream * int -> unit
	val outputLargeInt: outstream * LargeInt.int -> unit
	val outputLargeWord: outstream * LargeWord.word -> unit
	val outputLargeReal: outstream * LargeReal.real -> unit
	val outputAtom: outstream * string -> label
	val outputTuple: outstream * string * int -> label
	    (* followed by as many terms as the tuple is wide,
	     * in reverse order *)
	val outputList: (outstream * 'a -> 'b) -> outstream * 'a list -> label
	val outputBool: outstream * bool -> label
	val outputUnit: outstream -> label
	val outputString: outstream * string -> label
	val outputWord: outstream * int * LargeWord.word -> label
	val outputRef: (outstream * 'a -> 'b) -> outstream * 'a ref -> label
	val outputShared: outstream * label -> unit
    end
