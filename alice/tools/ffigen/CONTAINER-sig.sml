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

signature CONTAINER =
    sig
	val initialize : string -> unit

	val addFun : string * TypeTree.ty list * TypeTree.ty * string -> bool
	val addFun1 : string * TypeTree.ty list * TypeTree.ty * string -> bool
	val addConst : string -> string * int -> bool
	val addType : string * bool * string -> bool
	val addEnum : string * ((string*int) list) -> bool
	val addUserFun : Config.user_fun -> unit

	val getCType : TypeTree.ty -> string
	val getCDecl : string * TypeTree.ty -> string
	val getAliceType : TypeTree.ty -> string
	val cast : TypeTree.ty -> string -> string

	val saveBinding : string list -> (string * string) list -> (string * string) list -> unit
    end
