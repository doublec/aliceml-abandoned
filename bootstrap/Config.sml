(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001
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
end
