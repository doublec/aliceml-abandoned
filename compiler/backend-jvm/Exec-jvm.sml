(*
 * Author:
 *   Andy Walter <anwalt@ps.uni-sb.de>
 *
 * Copyright:
 *   Andy Walter, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

SMLofNJ.Internals.GC.messages false;
CM.make();

fun dmlc (_, debug::echo::x) =
    let
	val e=valOf (Int.fromString (String.substring(echo, 2,1)))
	fun dc (fi::rest) =
	    (if e >= 1 then print ("Compiling "^fi^"...\n") else ();
	    (CodeGen.genProgramCode
	     (valOf (Int.fromString (String.substring(debug, 2,1))),
	      e,
	      if (String.extract(fi, size fi-4, NONE)=".dml")
		  then String.substring(fi, 0, size fi-4)
	      else fi,
		  Main.imperatifyFile fi); dc (rest)))
	  | dc nil = 0
    in
	(dc x) handle _ => 1
    end
  | dmlc _ = 2

val _ = SMLofNJ.exportFn (".dmlc", dmlc)
