(*
 * Author:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

structure GlobalStamp :> GLOBAL_STAMP  =
struct
    datatype stamp		= GEN of int * Posix.ProcEnv.pid | STR of string
    type     t			= stamp

    val r			= ref 0

    fun stamp()			= (r := !r + 1; GEN(!r, Posix.ProcEnv.getpid()))

    fun fromString s		= STR s

    fun toString(GEN(n,_))	= Int.toString n
      | toString(STR s)		= s

    val equal			= op =

    fun hash(GEN(n,_))		= n
      | hash(STR s)		= String.hash s
end
