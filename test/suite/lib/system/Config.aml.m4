(*
 * Authors:
 *   Sebastian Germesin <germi@ps.uni-sb.de>
 *
 * Copyright:
 *   Sebastian Germesin, 2004
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)
changequote([[,]])

import signature TESTSUITE from "../../src/TESTSUITE-sig"

import structure Gen       from "x-alice:/lib/test/Gen"
import structure Test      from "../../src/Test"

import structure Date      from "x-alice:/lib/system/Date"
import structure Config    from "x-alice:/lib/system/Config"

structure TestConfig :> TESTSUITE = (* the config test suite *)
struct

    val randomize = Gen.randomize

    fun testPlatform () =
	ifdef([[WINDOWS]],[[
	      Test.test (fn x => x = Config.WIN32) Config.platform
	      ]],[[
	      Test.test (fn x => x = Config.UNIX) Config.platform
	      ]])

    fun testVM () = 
	ifdef([[SEAM]],[[
	      Test.test (fn s => s = "seam") Config.vm
	      ]],[[
	      Test.test (fn s => s <> "seam") Config.vm 
	      (*TODO: better differentationss? *)
	      ]])

    (* testBuildDate only tests, if Config.buildDate gives a valid date *)
    fun testBuildDate () =
	Test.test (fn x => (Date.toTime x; true) 
		         handle Date => false) Config.buildDate

    fun testPathEscape () =
	ifdef([[WINDOWS]],[[
	      Test.test (fn x => Option.isNone x) Config.pathEscape
	      ]],[[
	      Test.test (fn x => valOf x = #"\\"
			    handle Option => false) Config.pathEscape
	      ]])

    fun testPathEscape () = () (* TODO: how to implement? *)

    fun testPathSeparator () =
	ifdef([[WINDOWS]],[[
	      Test.test (fn x => x = #";") Config.pathSeparator
	      ]],[[
	      Test.test (fn x => x = #":") Config.pathSeparator
	      ]])
	    


    val suite = ("Config", [("vm", testVM),
			    ("platform", testPlatform),
			    ("buildDate", testBuildDate),
			    ("pathEscape", testPathEscape),
			    ("pathSeparator", testPathSeparator)])
end
