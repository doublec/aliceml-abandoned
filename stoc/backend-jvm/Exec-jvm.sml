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

fun dmlc (_, debug::verbose::optimize::tits::lmaa::lines::wait::x) =
    let
	val v=valOf (Int.fromString (String.substring(verbose, 2,1)))
	fun dc (fi::rest) =
	    (if v >= 1 then print ("compiling "^fi^"...\n") else ();
	    (CodeGen.genComponentCode
	     (valOf (Int.fromString (String.substring(debug, 2,1))),
	      v,
	      valOf (Int.fromString (String.substring(optimize, 2,1))),
	      valOf (Bool.fromString lmaa),
	      valOf (Bool.fromString lines),
	      valOf (Bool.fromString wait),
	      String.substring(fi, 0, size fi-4),
	      (if valOf (Bool.fromString tits)
		   then Main.imperatifyFile
	       else Main.imperatifyFile') fi);
	     dc (rest)))
	  | dc nil = 0
    in
	(dc x) handle _ => (print "compilation error.\n"; 1)
    end
  | dmlc _ = (print "unexpected parameter"; 2)

val _ = SMLofNJ.exportFn ("stoc-jvm", dmlc)
