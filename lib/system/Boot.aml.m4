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
import structure UnsafeOS    from "UnsafeOS"
import structure UnsafeValue from "UnsafeValue"

local
    __primitive val cast : 'a -> 'b = "Unsafe.cast" (* Thank Thorsten... *)
    val atExn = cast UnsafeOS.Process.atExn : (exn * unit -> unit) -> unit
    val _ = atExn (fn (exn, bt) =>
		   (TextIO.output (TextIO.stdErr, "alicerun: ");
		    case exn
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
					      {component, request, ...}) =>
			TextIO.output (TextIO.stdErr,
			    "failure type-checking " ^
			    Url.toString component ^
			    (case request of
				 NONE => "\n"
			       | SOME url => "\nas requested by " ^
					     Url.toString url ^ "\n"))
			     (*--** print cause *)
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
		      | _ =>
			TextIO.output (TextIO.stdErr,
			    "internal exception " ^ General.exnName exn ^ "\n");
		    TextIO.output (TextIO.stdErr, "Backtrace:\n");
		    UnsafeValue.dumpBacktrace bt;
		    UnsafeOS.Process.terminate UnsafeOS.Process.failure))
in
end
]],[[
]])

fun boot url = ComponentManager.start (Url.fromString url)
