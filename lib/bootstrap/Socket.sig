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

signature SOCKET_COMPONENT =
    sig
	signature SOCKET =
	    sig
		type t
		type vector = string
		type elem = char

		type host = string
		type port = int

		val server: port option * (t * host * port -> unit) -> t * port
		val client: host * port -> t

		val input1: t -> char option
		val inputN: t * int -> vector
		val output: t * vector -> unit
		val output1: t * char -> unit

		val close: t -> unit
	    end

	structure Socket: SOCKET
    end
