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
open String;

fun dmlc (_, debug::echo::x) =
    let
	fun dc (fi::rest) =
	    (CodeGen.genProgramCode
	     (valOf (Int.fromString (substring(debug, 2,1))),
	      valOf (Int.fromString (substring(echo, 2,1))),
	      if (extract(fi, size fi-4, NONE)=".dml")
		  then substring(fi, 0, size fi-4)
	      else fi,
		  Main.imperatifyFile fi); dc (rest))
	  | dc nil = 0
    in
	(dc x) handle _ => 1
    end
  | dmlc _ = 2

val _ = SMLofNJ.exportFn ("dmlc", dmlc)
