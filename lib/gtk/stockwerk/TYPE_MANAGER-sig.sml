
signature TYPE_MANAGER =
    sig
	datatype argtype = IN | OUT
	type arginfo = argtype * string * TypeTree.ty

	val removeTypeRefs :      TypeTree.ty -> TypeTree.ty

	val getCType :            TypeTree.ty -> string
	val getAliceType :        TypeTree.ty -> string
	val getAliceNativeType :  TypeTree.ty -> string

	val safeToUnsafe :        string -> TypeTree.ty -> string
        val unsafeToSafe :        string -> TypeTree.ty -> string

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

	val isRefOfSpace :        Util.spaces -> TypeTree.ty -> bool
	val isItemOfSpace :       Util.spaces -> TypeTree.decl -> bool
	val checkItem :           TypeTree.decl -> bool
	val checkStructMember :   TypeTree.struct_item -> bool
	val makeFieldFun :        Util.spaces 
	                            -> string * string * TypeTree.ty * bool
	                            -> string * TypeTree.ty * TypeTree.ty list
	val getEnumSpace :        string -> Util.spaces	
   end
