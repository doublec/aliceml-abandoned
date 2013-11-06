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

signature CRC =
    sig
	type t

	val initial: t
	val update: t * Word8.word -> t
	val toBytes: t -> Word8Vector.vector
    end
