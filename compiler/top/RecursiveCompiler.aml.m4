(*
 * Authors:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2005
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

changequote([[,]])

ifdef([[SEAM]],[[
import "SMLToSeamRecursiveCompiler"
]],[[
import "SMLToSeamMozartCompiler"
]])

structure RecursiveCompiler = RecursiveCompiler
