(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Skeleton :> SKELETON =
    struct
	fun module name =
	    ".module '" ^ name ^ ".dll'\n\
	    \.assembly '" ^ name ^ "' as \"" ^ name ^ ".dll\"\n\
	    \{\n\
	    \  .hash algorithm 0x0008004\n\
	    \  .ver 0:0:0:0\n\
	    \}\n"

	fun comtype (name, lib) =
	    ".comtype '" ^ name ^ "'\n\
	     \{\n\
	     \  .assembly extern '" ^ lib ^ "'\n\
	     \}\n"

	val externals =
	    ".assembly extern mscorlib\n\
	    \{\n\
	    \  .originator = (03 68 91 16 D3 A4 AE 33)\n\
	    \  .hash = (BD 5A 26 4D A5 B0 08 2D F5 9E 82 7B F0 5A B4 12\n\
	    \           BE 79 2B 43)\n\
	    \  .ver 0:0:0:0\n\
	    \}\n\
	    \.assembly extern Alice\n\
	    \{\n\
	    \  .hash = (36 2C FE 35 22 F7 53 22 67 E1 D1 88 E5 B3 9B E7\n\
	    \           B0 27 57 FB)\n\
	    \  .ver 0:0:0:0\n\
	    \}\n" ^
	    comtype ("System.Object", "mscorlib") ^
	    comtype ("System.Int32", "mscorlib") ^
	    comtype ("System.Char", "mscorlib") ^
	    comtype ("System.String", "mscorlib") ^
	    comtype ("System.Double", "mscorlib") ^
	    comtype ("System.Guid", "mscorlib") ^
	    comtype ("System.Array", "mscorlib") ^
	    comtype ("Alice.Values.Procedure", "Alice") ^
	    comtype ("Alice.Values.Procedure0", "Alice") ^
	    comtype ("Alice.Values.Procedure2", "Alice") ^
	    comtype ("Alice.Values.Procedure3", "Alice") ^
	    comtype ("Alice.Values.Procedure4", "Alice") ^
	    comtype ("Alice.Values.Procedure5", "Alice") ^
	    comtype ("Alice.Values.Procedure6", "Alice") ^
	    comtype ("Alice.Values.Procedure7", "Alice") ^
	    comtype ("Alice.Values.Procedure8", "Alice") ^
	    comtype ("Alice.Values.Procedure9", "Alice") ^
	    comtype ("Alice.Values.Cell", "Alice") ^
	    comtype ("Alice.Values.Future", "Alice") ^
	    comtype ("Alice.Values.TagVal", "Alice") ^
	    comtype ("Alice.Values.ConVal", "Alice") ^
	    comtype ("Alice.Values.Exception", "Alice") ^
	    comtype ("Alice.Values.TagConstructor", "Alice") ^
	    comtype ("Alice.Values.ConConstructor", "Alice") ^
	    comtype ("Alice.Values.Selector", "Alice") ^
	    comtype ("Alice.Builtins.opeq", "Alice") ^
	    comtype ("Alice.Builtins.opnoteq", "Alice") ^
	    comtype ("Alice.Builtins.Array_array", "Alice") ^
	    comtype ("Alice.Builtins.Array_fromList", "Alice") ^
	    comtype ("Alice.Builtins.Array_length", "Alice") ^
	    comtype ("Alice.Builtins.Array_sub", "Alice") ^
	    comtype ("Alice.Builtins.Array_update", "Alice") ^
	    comtype ("Alice.Builtins.Char_opless", "Alice") ^
	    comtype ("Alice.Builtins.Char_opgreater", "Alice") ^
	    comtype ("Alice.Builtins.Char_oplessEq", "Alice") ^
	    comtype ("Alice.Builtins.Char_opgreaterEq", "Alice") ^
	    comtype ("Alice.Builtins.Char_ord", "Alice") ^
	    comtype ("Alice.Builtins.Char_chr", "Alice") ^
	    comtype ("Alice.Builtins.Char_isAlpha", "Alice") ^
	    comtype ("Alice.Builtins.Char_isAlphaNum", "Alice") ^
	    comtype ("Alice.Builtins.Char_isDigit", "Alice") ^
	    comtype ("Alice.Builtins.Char_isGraph", "Alice") ^
	    comtype ("Alice.Builtins.Char_isHexDigit", "Alice") ^
	    comtype ("Alice.Builtins.Char_isLower", "Alice") ^
	    comtype ("Alice.Builtins.Char_isPrint", "Alice") ^
	    comtype ("Alice.Builtins.Char_isPunct", "Alice") ^
	    comtype ("Alice.Builtins.Char_isSpace", "Alice") ^
	    comtype ("Alice.Builtins.Char_isUpper", "Alice") ^
	    comtype ("Alice.Builtins.Char_toLower", "Alice") ^
	    comtype ("Alice.Builtins.Char_toUpper", "Alice") ^
	    comtype ("Alice.Builtins.General_assign", "Alice") ^
	    comtype ("Alice.Builtins.General_exchange", "Alice") ^
	    comtype ("Alice.Builtins.General_exnName", "Alice") ^
	    comtype ("Alice.Builtins.Future_alarmQuote", "Alice") ^
	    comtype ("Alice.Builtins.Future_await", "Alice") ^
	    comtype ("Alice.Builtins.Future_awaitOne", "Alice") ^
	    comtype ("Alice.Builtins.Future_byneed", "Alice") ^
	    comtype ("Alice.Builtins.Future_concur", "Alice") ^
	    comtype ("Alice.Builtins.Future_isFailed", "Alice") ^
	    comtype ("Alice.Builtins.Future_isFuture", "Alice") ^
	    comtype ("Alice.Builtins.GlobalStamp_new", "Alice") ^
	    comtype ("Alice.Builtins.GlobalStamp_fromString", "Alice") ^
	    comtype ("Alice.Builtins.GlobalStamp_toString", "Alice") ^
	    comtype ("Alice.Builtins.GlobalStamp_compare", "Alice") ^
	    comtype ("Alice.Builtins.GlobalStamp_hash", "Alice") ^
	    comtype ("Alice.Builtins.Hole_fail", "Alice") ^
	    comtype ("Alice.Builtins.Hole_fill", "Alice") ^
	    comtype ("Alice.Builtins.Hole_future", "Alice") ^
	    comtype ("Alice.Builtins.Hole_hole", "Alice") ^
	    comtype ("Alice.Builtins.Hole_isFailed", "Alice") ^
	    comtype ("Alice.Builtins.Hole_isFree", "Alice") ^
	    comtype ("Alice.Builtins.Int_opnegate", "Alice") ^
	    comtype ("Alice.Builtins.Int_opadd", "Alice") ^
	    comtype ("Alice.Builtins.Int_opsub", "Alice") ^
	    comtype ("Alice.Builtins.Int_opmul", "Alice") ^
	    comtype ("Alice.Builtins.Int_opless", "Alice") ^
	    comtype ("Alice.Builtins.Int_opgreater", "Alice") ^
	    comtype ("Alice.Builtins.Int_oplessEq", "Alice") ^
	    comtype ("Alice.Builtins.Int_opgreaterEq", "Alice") ^
	    comtype ("Alice.Builtins.Int_abs", "Alice") ^
	    comtype ("Alice.Builtins.Int_compare", "Alice") ^
	    comtype ("Alice.Builtins.Int_div", "Alice") ^
	    comtype ("Alice.Builtins.Int_mod", "Alice") ^
	    comtype ("Alice.Builtins.Int_quot", "Alice") ^
	    comtype ("Alice.Builtins.Int_rem", "Alice") ^
	    comtype ("Alice.Builtins.Int_toString", "Alice") ^
	    comtype ("Alice.Builtins.Math_acos", "Alice") ^
	    comtype ("Alice.Builtins.Math_acosh", "Alice") ^
	    comtype ("Alice.Builtins.Math_asin", "Alice") ^
	    comtype ("Alice.Builtins.Math_asinh", "Alice") ^
	    comtype ("Alice.Builtins.Math_atan", "Alice") ^
	    comtype ("Alice.Builtins.Math_atanh", "Alice") ^
	    comtype ("Alice.Builtins.Math_atan2", "Alice") ^
	    comtype ("Alice.Builtins.Math_cos", "Alice") ^
	    comtype ("Alice.Builtins.Math_cosh", "Alice") ^
	    comtype ("Alice.Builtins.Math_exp", "Alice") ^
	    comtype ("Alice.Builtins.Math_ln", "Alice") ^
	    comtype ("Alice.Builtins.Math_pow", "Alice") ^
	    comtype ("Alice.Builtins.Math_sin", "Alice") ^
	    comtype ("Alice.Builtins.Math_sinh", "Alice") ^
	    comtype ("Alice.Builtins.Math_sqrt", "Alice") ^
	    comtype ("Alice.Builtins.Math_tan", "Alice") ^
	    comtype ("Alice.Builtins.Math_tanh", "Alice") ^
	    comtype ("Alice.Builtins.Real_opnegate", "Alice") ^
	    comtype ("Alice.Builtins.Real_opadd", "Alice") ^
	    comtype ("Alice.Builtins.Real_opsub", "Alice") ^
	    comtype ("Alice.Builtins.Real_opmul", "Alice") ^
	    comtype ("Alice.Builtins.Real_opdiv", "Alice") ^
	    comtype ("Alice.Builtins.Real_opless", "Alice") ^
	    comtype ("Alice.Builtins.Real_opgreater", "Alice") ^
	    comtype ("Alice.Builtins.Real_oplessEq", "Alice") ^
	    comtype ("Alice.Builtins.Real_opgreaterEq", "Alice") ^
	    comtype ("Alice.Builtins.Real_ceil", "Alice") ^
	    comtype ("Alice.Builtins.Real_compare", "Alice") ^
	    comtype ("Alice.Builtins.Real_floor", "Alice") ^
	    comtype ("Alice.Builtins.Real_fromInt", "Alice") ^
	    comtype ("Alice.Builtins.Real_realCeil", "Alice") ^
	    comtype ("Alice.Builtins.Real_realFloor", "Alice") ^
	    comtype ("Alice.Builtins.Real_realTrunc", "Alice") ^
	    comtype ("Alice.Builtins.Real_rem", "Alice") ^
	    comtype ("Alice.Builtins.Real_round", "Alice") ^
	    comtype ("Alice.Builtins.Real_toString", "Alice") ^
	    comtype ("Alice.Builtins.Real_trunc", "Alice") ^
	    comtype ("Alice.Builtins.String_append", "Alice") ^
	    comtype ("Alice.Builtins.String_opless", "Alice") ^
	    comtype ("Alice.Builtins.String_opgreater", "Alice") ^
	    comtype ("Alice.Builtins.String_oplessEq", "Alice") ^
	    comtype ("Alice.Builtins.String_opgreaterEq", "Alice") ^
	    comtype ("Alice.Builtins.String_compare", "Alice") ^
	    comtype ("Alice.Builtins.String_explode", "Alice") ^
	    comtype ("Alice.Builtins.String_implode", "Alice") ^
	    comtype ("Alice.Builtins.String_size", "Alice") ^
	    comtype ("Alice.Builtins.String_sub", "Alice") ^
	    comtype ("Alice.Builtins.String_substring", "Alice") ^
	    comtype ("Alice.Builtins.String_str", "Alice") ^
	    comtype ("Alice.Builtins.Thread_Terminate", "Alice") ^
	    comtype ("Alice.Builtins.Thread_current", "Alice") ^
	    comtype ("Alice.Builtins.Thread_isSuspended", "Alice") ^
	    comtype ("Alice.Builtins.Thread_raiseIn", "Alice") ^
	    comtype ("Alice.Builtins.Thread_resume", "Alice") ^
	    comtype ("Alice.Builtins.Thread_state", "Alice") ^
	    comtype ("Alice.Builtins.Thread_suspend", "Alice") ^
	    comtype ("Alice.Builtins.Thread_yield", "Alice") ^
	    comtype ("Alice.Builtins.Unsafe_Array_sub", "Alice") ^
	    comtype ("Alice.Builtins.Unsafe_Array_update", "Alice") ^
	    comtype ("Alice.Builtins.Unsafe_String_sub", "Alice") ^
	    comtype ("Alice.Builtins.Unsafe_Vector_sub", "Alice") ^
	    comtype ("Alice.Builtins.Unsafe_cast", "Alice") ^
	    comtype ("Alice.Builtins.Vector_fromList", "Alice") ^
	    comtype ("Alice.Builtins.Vector_length", "Alice") ^
	    comtype ("Alice.Builtins.Vector_sub", "Alice") ^
	    comtype ("Alice.Builtins.Word_fromIntQuote", "Alice") ^
	    comtype ("Alice.Builtins.Word_fromInt", "Alice") ^
	    comtype ("Alice.Builtins.Word_toInt", "Alice") ^
	    comtype ("Alice.Builtins.Word_toIntX", "Alice") ^
	    comtype ("Alice.Builtins.Word_opadd", "Alice") ^
	    comtype ("Alice.Builtins.Word_opsub", "Alice") ^
	    comtype ("Alice.Builtins.Word_opmul", "Alice") ^
	    comtype ("Alice.Builtins.Word_div", "Alice") ^
	    comtype ("Alice.Builtins.Word_mod", "Alice") ^
	    comtype ("Alice.Builtins.Word_orb", "Alice") ^
	    comtype ("Alice.Builtins.Word_xorb", "Alice") ^
	    comtype ("Alice.Builtins.Word_andb", "Alice") ^
	    comtype ("Alice.Builtins.Word_notb", "Alice") ^
	    comtype ("Alice.Builtins.Word_shl", "Alice") ^
	    comtype ("Alice.Builtins.Word_shr", "Alice") ^
	    comtype ("Alice.Builtins.Word_arithshr", "Alice") ^
	    comtype ("Alice.Builtins.Word_toString", "Alice") ^
	    comtype ("Alice.Builtins.OS_Process_system", "Alice") ^
	    comtype ("Alice.Builtins.OS_Process_exit", "Alice") ^
	    comtype ("Alice.Builtins.OS_Process_getEnv", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_openIn", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_inputAll", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_inputLine", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_closeIn", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_openOut", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_output", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_output1", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_flushOut", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_closeOut", "Alice") ^
	    comtype ("Alice.Builtins.TextIO_print", "Alice") ^
	    comtype ("Alice.Builtins.CommandLine_name", "Alice") ^
	    comtype ("Alice.Builtins.CommandLine_arguments", "Alice") ^
	    comtype ("Alice.Prebound", "Alice") ^
	    comtype ("Alice.Komponist", "Alice")
    end
