(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001-2003
 *   Andreas Rossberg, 2001-2005
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Config =
struct
    datatype platform = WIN32 | UNIX

    val platform =
	case SMLofNJ.SysInfo.getOSKind () of
	    SMLofNJ.SysInfo.WIN32 => WIN32
	  | _ => UNIX

    val vm = "smlnj"

    fun homeDir () =
	case OS.Process.getEnv "ALICE_HOME" of
	    SOME s => s
	  | NONE => OS.FileSys.getDir ()

    val pathEscape =
	case platform of
	    WIN32 => NONE
	  | UNIX => SOME #"\\"

    val pathSeparator =
	case platform of
	    WIN32 => #";"
	  | UNIX => #":"

    val version = {major=0, minor=0, revision=0}
    val codename = "'New Jersey Hits The Decks' Bootstrap Mix"
    val buildDate = Date.fromTimeLocal (Time.now ())
end
