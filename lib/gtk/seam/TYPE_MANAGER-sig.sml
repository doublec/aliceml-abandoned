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

signature TYPE_MANAGER =
    sig
	exception EStruct
	exception EUnion

	val removeTypeRefs :      TypeTree.ty -> TypeTree.ty

	val isRefOfSpace :        Util.spaces -> TypeTree.ty -> bool
	val isItemOfSpace :       Util.spaces -> TypeTree.decl -> bool

	val buildClassList :      TypeTree.tree -> unit

	val getCType :            TypeTree.ty -> string
	val getAliceType :        TypeTree.ty -> string
	val getAliceNativeType :  TypeTree.ty -> string

	val safeToUnsafe :        string -> TypeTree.ty -> string
        val unsafeToSafe :        string -> TypeTree.ty -> string

	val fromWord :            TypeTree.ty -> string * string list
        val toWord :              TypeTree.ty -> string * string list
        val outInit :             TypeTree.ty -> string

	datatype argtype = IN | OUT
	type arginfo = argtype * string * TypeTree.ty

	val splitArgTypes :       TypeTree.ty list -> arginfo list
	val splitArgTypesNoOuts : TypeTree.ty list -> arginfo list
	val splitInOuts :         arginfo list * bool -> 
	                                arginfo list * arginfo list
        val numIns :              arginfo list * bool -> int
        val numOuts :             arginfo list * bool -> int

	val getCFunType :         string * TypeTree.ty * arginfo list * bool 
 	                               -> string
	val getAliceFunType :     string * TypeTree.ty * arginfo list * bool
	                               -> (TypeTree.ty -> string) -> string
	val makeFieldFun :        Util.spaces 
	                            -> string * string * TypeTree.ty * bool
	                            -> string * TypeTree.ty * TypeTree.ty list

	val checkItem :           TypeTree.decl -> bool
	val checkStructMember :   TypeTree.struct_item -> bool
	val checkEnumMember :     TypeTree.enum_item -> bool

   end
