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

structure Abbrev  =
    struct
	open JVMInst

	(* java classes *)
	val CObject           = "java/lang/Object"
	val CPrintStream   = "java/io/PrintStream"
	val CString        = "java/lang/String"
	val CThread        = "java/lang/Thread"
	val CVector        = "java/util/Vector"
	val CClass         = "java/lang/Class"
	val CCompilerException = "java/lang/RuntimeException"

	(* java methods *)
	val MForName       = (CClass, "forName", ([Classsig CString], [Classsig CClass]))
	val MPrint = (CPrintStream, "print", ([Classsig CObject], [Voidsig]))
	val MPrintln = (CPrintStream, "println", ([Classsig CObject], [Voidsig]))
	val MEquals = (CString,"equals", ([Classsig CObject],[Boolsig]))

	(* java fields *)
	val FOut           = ("java/lang/System/out", [Classsig CPrintStream])

	(* runtime classes *)
	val CName          = "de/uni_sb/ps/dml/runtime/Name"
	val CConstructor   = "de/uni_sb/ps/dml/runtime/Constructor"
	val CConVal        = "de/uni_sb/ps/dml/runtime/ConVal"
	fun cConVal n      = CConVal^(if n=1 orelse n>=5 then "" else Int.toString n)
	val CExWrap        = "de/uni_sb/ps/dml/runtime/ExceptionWrapper"
	val CFcnClosure    = "de/uni_sb/ps/dml/runtime/Function"
	val CInt           = "de/uni_sb/ps/dml/runtime/Int"
	val CWord          = "de/uni_sb/ps/dml/runtime/Word"
	val CChar          = "de/uni_sb/ps/dml/runtime/Char"
	val CReal          = "de/uni_sb/ps/dml/runtime/Real"
	val CRecord        = "de/uni_sb/ps/dml/runtime/Record"
	val CStr           = "de/uni_sb/ps/dml/runtime/String"
	val CTuple         = "de/uni_sb/ps/dml/runtime/Tuple"
	val CBuiltin       = "de/uni_sb/ps/dml/runtime/Builtin"
	val CSelString     = "de/uni_sb/ps/dml/runtime/General$SelFunString"
	val CSelInt        = "de/uni_sb/ps/dml/runtime/General$SelFunInt"
	val CReference     = "de/uni_sb/ps/dml/runtime/Reference"
	fun cTuple n       = CTuple^(if n=1 orelse n >= 5 then "" else Int.toString n)

	(* runtime interfaces *)
	val IConVal        = "de/uni_sb/ps/dml/runtime/DMLConVal"
	val ITransient  = "de/uni_sb/ps/dml/runtime/DMLTransient"
	val IVal           = "de/uni_sb/ps/dml/runtime/DMLValue"
	val ITuple      = "de/uni_sb/ps/dml/runtime/DMLTuple"

	(* some helper functions *)
	fun valList 0 = nil
	  | valList n = Classsig IVal :: valList (n-1)

	(* generate names for apply methods. *)
	fun applyName 1 = "apply"
	  | applyName parms =
	    "apply"^Int.toString (if parms <=4 then parms else 1)

	(* runtime methods *)
	val MRequest       = (ITransient, "request", ([], [Classsig IVal]))
	val MPickle = ("de/uni_sb/ps/dml/runtime/General$Pickle","apply",
		       ([Classsig CString, Classsig IVal, Classsig IVal], [Classsig IVal]))
	val MApply = (IVal, "apply", ([Classsig IVal], [Classsig IVal]))
	fun mApply n = (IVal, applyName n, (valList n, [Classsig IVal]))
	val MGetBuiltin = (CBuiltin, "getBuiltin", ([Classsig CStr], [Classsig IVal]))

	(* runtime builtins *)
	val BUnit          = ("de/uni_sb/ps/dml/runtime/Constants/dmlunit",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val BMatch         = ("de/uni_sb/ps/dml/runtime/General/Match",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val BFalse         = ("de/uni_sb/ps/dml/runtime/Constants/dmlfalse",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val BTrue          = ("de/uni_sb/ps/dml/runtime/Constants/dmltrue",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val BNil          = ("de/uni_sb/ps/dml/runtime/List/nil",
			     [Classsig "de/uni_sb/ps/dml/runtime/Name"])
	val BCons          = ("de/uni_sb/ps/dml/runtime/List/cons",
			      [Classsig "de/uni_sb/ps/dml/runtime/Constructor"])
	val BRef          = ("de/uni_sb/ps/dml/runtime/Constants/reference",
			     [Classsig "de/uni_sb/ps/dml/runtime/Constructor"])
	val BBind          = ("de/uni_sb/ps/dml/runtime/General/Bind",
			      [Classsig "de/uni_sb/ps/dml/runtime/Name"])
    end
