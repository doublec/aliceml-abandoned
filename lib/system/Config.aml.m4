(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 2001-2004
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

import structure Date         from "Date"
import structure OS           from "OS"
import signature CONFIG       from "CONFIG-sig"
import structure UnsafeConfig from "UnsafeConfig"

structure Config : CONFIG =
struct
    datatype platform = WIN32 | UNIX

    val platform = UnsafeConfig.platform
    val vm = UnsafeConfig.vm

(*--** obtain from UnsafeConfig; normalize *)
    fun homeDir () = Option.valOf (OS.Process.getEnv "ALICE_HOME")

    val pathEscape =
	case platform of
	    WIN32 => NONE
	  | UNIX => SOME #"\\"

    val pathSeparator =
	case platform of
	    WIN32 => #";"
	  | UNIX => #":"

    val buildDate = valOf (Date.fromISO ("substr(esyscmd(date "-I"), 0, 10)"))
end
