(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000-2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Posix =
    struct
	structure ProcEnv =
	    struct
		type pid = int

		fun getpid () = 0
	    end
    end
