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

	val externals =
	    ".assembly extern mscorlib\n\
	    \{\n\
	    \  .originator = (03 68 91 16 D3 A4 AE 33)\n\
            \  .hash = (E6 60 D4 00 69 E0 27 86 19 76 23 3A FC 8D A2 7B\n\
            \           72 17 F0 96)\n\
	    \}\n\
	    \.assembly extern Alice\n\
	    \{\n\
            \  .hash = (ED 04 00 97 BC B8 92 9D 7B A2 3D 57 5C 57 BE 0D\n\
            \           19 11 BB A5)\n\ 
	    \  .ver 0:0:0:0\n\
	    \}\n"
    end
