(*
 * Authors:
 *   Robert Grabowski <grabow@ps.uni-sb.de>
 *
 * Copyright:
 *   Robert Grabowski, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature SPECIAL =
    sig
	val includeFile :   string * string * int

        val specialFuns :    TypeTree.decl list
        val changedFuns :    TypeTree.decl list

        val isIgnored :      TypeTree.decl -> bool
    end
