(*
 * Authors:
 *   Robert Grabowski <grabow@ps.uni-sb.de>
 *
 * Copyright:
 *   Robert Grabowski, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

(* 
  This structure contains constructors for the intermediate type 
  representation.
*)

structure TypeTree =
   struct
      datatype num_kind = 
	 CHAR
       | SHORT
       | INT
       | LONG
       | LONGLONG
       | FLOAT
       | DOUBLE
       | LONGDOUBLE 

      datatype ty =
	 VOID
       | ELLIPSES of bool          (* if true: ..., if false: va_list *)
       | BOOL
       | NUMERIC of bool * bool * num_kind  (* signed? / real? / kind *)
       | POINTER of ty
       | STRING of bool                     (* pointer to signed char? *)
       | ARRAY of int option * ty           (* size / type *)
       | LIST of string * ty                (* c_type_name / elem_type *)
       | FUNCTION of ty * ty list           (* retval / arglist *)
       | STRUCTREF of string
       | UNIONREF of string
       | ENUMREF of string
       | TYPEREF of string * ty             (* c_type_name / type *)

      type struct_item = string * ty           (* field_name/type *)
      type enum_item   = string * LargeInt.int (* field_name/index *)
    
      datatype decl =
	 FUNC   of string * ty * ty list     (* function_name/retval/arglist *)
       | STRUCT of string * struct_item list (* struct_name/struct_items *)
       | UNION  of string * struct_item list (* union_name/struct_items *)
       | ENUM   of string * enum_item list   (* enum_name/struct_items *)
       | ALIAS  of string * ty               (* typedef_name/type *)

      type tree = decl list

(* Notes:
   - The second component of the NUMERIC constructor (indicating a floating
     point number) is redundant to the num_kind information.
   - The char/uchar information in the STRING constructor is needed for
     a correct C type cast in the generated native code.
   - The ARRAY size is an int option, because its size may be undefined
     (like gchar[]).
   - The LIST's c_type_name may be "GList" or "GSList".
   - The FUNCTION constructor is for function pointers (not really used
     in this binding), while the FUNC constructor is for function declarations.
   - The c_type_name of a TYPEREF is needed for correct C casts in the
     generated native code.    
*)
end
