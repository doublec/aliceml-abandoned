(*
 * Author:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 2001-2004
 *
 * Last change:
 *   $Date$ $Author$
 *   $Revision$
 *)

import structure Url              from "Url"
import structure Component        from "Component"
import structure ComponentManager from "ComponentManager"

changequote([[,]])

ifdef([[SEAM]],[[
import structure IO          from "IO"
import structure TextIO      from "TextIO"
import structure OS          from "OS"

import structure PPMismatch  from "../rtt/PPMismatch"
import structure PrettyPrint from "../utility/PrettyPrint"

local
    val _ = OS.Process.atExn
	    (fn packet =>
		(TextIO.output (TextIO.stdErr, "alicerun: ");
		 case Exn.fromPacket packet
		  of Component.Failure (url, Component.Eval
					     (Assert (file, line))) =>
		     TextIO.output (TextIO.stdErr,
			 "assertion failure at " ^ file ^ ":" ^
			 Int.toString line ^ "\n")
		   | Component.Failure (url, Component.Eval exn) =>
		     TextIO.output (TextIO.stdErr,
			 "uncaught exception " ^ General.exnName exn ^
			 " while evaluating " ^ Url.toString url ^ "\n")
		   | Component.Failure (_, Component.Mismatch
					   {component, request, cause}) =>
		     let
			 val s = "failure type-checking " ^
				 Url.toString component ^
				 (case request of
				      NONE => "\n"
				    | SOME url => "\nas requested by " ^
						  Url.toString url ^ "\n") ^
				 "Reason:"
		     in
			 PrettyPrint.output
			 (TextIO.stdErr,
			  PPMismatch.ppMismatch' (PrettyPrint.text s, cause),
			  79);
			 TextIO.output (TextIO.stdErr, "\n")
		     end
		   | Component.Failure (url, IO.Io _) =>
		     TextIO.output (TextIO.stdErr,
			 "failure loading " ^  Url.toString url ^ "\n")
		   | Component.Failure (url, Component.Internal exn) =>
		     TextIO.output (TextIO.stdErr,
			 "internal exception " ^ General.exnName exn ^
			 " while linking " ^ Url.toString url ^ "\n")
		   | Component.Failure (url, exn) =>
		     TextIO.output (TextIO.stdErr,
			 "unknown failure " ^ General.exnName exn ^
			 " while linking " ^ Url.toString url ^ "\n")
		   | Assert (file, line) =>
		     TextIO.output (TextIO.stdErr,
			 "internal assertion failure at " ^ file ^ ":" ^
			 Int.toString line ^ "\n")
		   | exn =>
		     TextIO.output (TextIO.stdErr,
			 "internal exception " ^ General.exnName exn ^ "\n");
		 TextIO.output (TextIO.stdErr, "Backtrace:\n");
		 Exn.dumpTrace packet;
		 OS.Process.terminate OS.Process.failure))
in
end
]],[[
]])

fun boot url = ignore (ComponentManager.link (Url.fromString url))
