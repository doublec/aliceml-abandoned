
structure TypeTree =
   struct
      exception EStruct
      exception EUnion
      exception EFunction
      exception EArray
      exception EEllipses

      datatype numKind = 
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
       | ELLIPSES
       | BOOL
       | NUMERIC of bool * bool * numKind  (* signed? / real? / kind *)
       | POINTER of ty
       | STRING of bool    (* pointer to signed char? *)
       | ARRAY of LargeInt.int option * ty (* size / type *)
       | LIST of string * ty (* c_type_name / elem_type *)
       | FUNCTION of ty * ty list
       | STRUCTREF of string
       | UNIONREF of string
       | ENUMREF of string
       | TYPEREF of string * ty     (* c_type_name / type *)

      type struct_item = string * ty           (* field_name/type *)
      type enum_item   = string * LargeInt.int (* field_name/index *)
    
      datatype decl =
	 FUNC   of string * ty * ty list (* name/retval/arglist *)
       | STRUCT of string * struct_item list (* name/struct_items *)
       | UNION  of string * struct_item list (* name/struct_items *)
       | ENUM   of string * enum_item list (* name/struct_items *)
       | ALIAS  of string * ty     (* typedef_name/type *)
       | CLASS  of string * string (* parent_name/own_name *)

      type tree = decl list

end
