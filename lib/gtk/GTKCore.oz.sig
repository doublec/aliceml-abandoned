(*
 * Authors:
 *   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
 *
 * Copyright:
 *   Thorsten Brunklaus, 2000
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature GTK_CORE_COMPONENT =
    sig
	signature GTK_CORE =
	    sig
		type object

                val pointerToObject : object -> int
                val objectToPointer : int -> object
                val removeObject : int -> unit
                val signalConnect : object * string * (unit -> unit) -> int
                val signalDisconnect : object * int -> unit
                val signalHandlerBlock : object * int -> unit
                val signalHandlerUnblock : object * int -> unit
                val signalEmit : object * string -> unit
		val exit : unit -> unit
	    end

	structure GTKCore : GTK_CORE
    end
