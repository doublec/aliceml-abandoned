(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001-2003
 *   Andreas Rossberg, 2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

signature CONFIG =
sig
    datatype platform = WIN32 | UNIX

    val platform : platform
    val vm : string

    val homeDir : string option

    val pathEscape : char option
    val pathSeparator : char
end

structure Config : CONFIG =
struct
    datatype platform = WIN32 | UNIX

    val platform =
	case SMLofNJ.SysInfo.getOSKind () of
	    SMLofNJ.SysInfo.WIN32 => WIN32
	  | _ => UNIX

    val vm = "smlnj"

    val homeDir = OS.Process.getEnv "STOCKHOME"

    val pathEscape =
	case platform of
	    WIN32 => NONE
	  | UNIX => SOME #"\\"

    val pathSeparator =
	case platform of
	    WIN32 => #";"
	  | UNIX => #":"
end
