(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 2001-2004
 *
 * Last change:
 *   $Date$ $Author$
 *   $Revision$
 *)

import structure Url              from "Url"
import structure ComponentManager from "ComponentManager"

changequote([[,]])

ifdef([[SEAM]],[[
import structure TextIO      from "TextIO"
import structure UnsafeOS    from "UnsafeOS"
import structure UnsafeValue from "UnsafeValue"

local
    __primitive val cast : 'a -> 'b = "Unsafe.cast" (* Thank Thorsten... *)
    val atExn = cast UnsafeOS.Process.atExn : (exn * unit -> unit) -> unit
    val _ = atExn (fn (e, bt) =>
			(TextIO.output (TextIO.stdErr, "Uncaught exception " ^
							exnName e ^
							"\nBacktrace:\n");
			 UnsafeValue.dumpBacktrace bt;
			 UnsafeOS.Process.terminate ()))
in
end
]],[[
]])

fun boot url = ComponentManager.start (Url.fromString url)
