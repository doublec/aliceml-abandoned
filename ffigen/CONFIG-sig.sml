(*
 * Authors:
 *   Sven Woop <woop@ps.uni-sb.de>
 *
 * Copyright:
 *   Sven Woop, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature CONFIG =
    sig
	type type_descr = {alicetype : string, toword : string, fromword : string } 

	type user_fun = {name : string, typ : string,
		     export : string, body : string }

	datatype filter_cmd =
	    CMD_REJECT
	  | CMD_ACCEPT of string

	val readCFGFile : string -> unit
	val applyFilter : string -> filter_cmd
	val isSpecialType : string -> type_descr option
	val getUserFuns : unit -> user_fun list
	val handleAsInt : string -> bool
	val getTypeDep : unit -> (string * bool * string) list
	val getImports : unit -> (string * string) list
    end
