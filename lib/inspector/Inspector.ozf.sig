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

signature INSPECTOR_COMPONENT =
    sig
	signature INSPECTOR =
	    sig
		val inspect : 'a -> unit
		val inspectN : int * 'a -> unit
		val configure : 'a -> unit
	    end

	structure Inspector : INSPECTOR
    end
