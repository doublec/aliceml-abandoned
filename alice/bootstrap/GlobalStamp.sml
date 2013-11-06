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

    fun compare(STR s1, STR s2)	= String.compare(s1,s2)
      | compare(STR _,  GEN _)  = LESS
      | compare(GEN _,  STR _)  = GREATER
      | compare(GEN(n1,_), GEN(n2,_)) = Int.compare(n1,n2)	(* Hack! *)
end
