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

structure Skeleton :> SKELETON =
    struct
	fun module name =
	    ".module '" ^ name ^ ".dll'\n\
	    \.assembly '" ^ name ^ "' as \"" ^ name ^ ".dll\"\n\
	    \{\n\
	    \  .hash algorithm 0x0008004\n\
	    \  .ver 0:0:0:0\n\
	    \}\n"

	fun readFile filename =
	    let
		val file   = TextIO.openIn filename
		val source = TextIO.inputAll file
		val _      = TextIO.closeIn file
	    in
		source
	    end

	fun externals () =
	    let
		val stockhome =
		    case OS.Process.getEnv "STOCKHOME" of
			SOME s => s ^ "/"
		      | NONE => ""
	    in
		readFile (stockhome ^ "Skeleton.il")
	    end
    end
