(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2001
 *   Andreas Rossberg, 2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

import structure OS from "OS"
import signature CONFIG from "CONFIG-sig"
import structure NativeConfig from "NativeConfig"

structure Config : CONFIG =
struct
    datatype platform = WIN32 | UNIX

    val platform = NativeConfig.platform
    val homeDir = OS.Process.getEnv "STOCKHOME"
end
