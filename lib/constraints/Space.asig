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

signature SPACE_COMPONENT =
    sig
	signature SPACE =
	    sig
		type 'a space

		datatype status =
		    FAILED
		  | SUCCEEDED
		  | ALTERNATIVES of int

		datatype choice =
		    SINGLE of int
		  | RANGE  of int * int

		val space : (unit -> 'a) -> 'a space

		val ask : 'a space -> status
		val clone : 'a space -> 'a space
		val commit : 'a space * choice -> unit
		val inject : 'a space * ('a -> unit) -> unit
		val merge : 'a space -> 'a

		(* to be replaced soon *)
		val eq : 'a space * 'a space -> bool
	    end

	structure Space : SPACE
    end
