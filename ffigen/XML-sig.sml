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

signature XML =
    sig

	datatype tag = 
	    T of string * tagentry
	and tagentry = 
	    STR of string 
	  | CHILDS of tag list

	val xml_get_child : string * tag -> tag list
	val xml_get_childs : tag -> tag list
	val xml_load : string -> tag

    end
