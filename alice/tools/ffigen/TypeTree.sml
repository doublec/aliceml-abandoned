(*
 * Authors:
 *   Sven Woop <woop@ps.uni-sb.de>
 *
 * Copyright:
 *   Sven Woop, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)


structure TypeTree =
struct
    datatype sign = SIGNED | UNSIGNED   
	
    datatype ty =
	CHAR of sign
      | SHORT of sign
      | INT of sign
      | LONG of sign
      | LONGLONG of sign
      | FLOAT
      | DOUBLE
      | LONGDOUBLE 
      | VOID
      | CONST of ty
      | ELLIPSES of bool          (* if true: ..., if false: va_list *)
      | BOOL
      | POINTER of ty
      | STRING
      | ARRAY of int option * ty           (* size / type *)
      | LIST of ty         
      | FUNCTION of ty * ty list           (* retval / arglist *)
      | STRUCTREF of string
      | UNIONREF of string
      | ENUMREF of string
      | TYPEREF of string * ty
      | TYPE_VAR of int option
	
    type struct_item = string * ty         (* field_name/type *)
    type enum_item   = string * int        (* field_name/index *)
	
    datatype decl =
	FUNC   of string * ty * ty list     (* function_name/retval/arglist *)
      | STRUCT of string * struct_item list (* struct_name/struct_items *)
      | UNION  of string * struct_item list (* union_name/struct_items *)
      | ENUM   of string * enum_item list   (* enum_name/struct_items *)
      | ALIAS  of string * ty               (* typedef_name/type *)
	
    type tree = decl list

end
