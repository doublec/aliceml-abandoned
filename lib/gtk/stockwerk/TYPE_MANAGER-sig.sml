
signature TYPE_MANAGER =
    sig
	datatype argtype = IN | OUT
	type arginfo = argtype * string * TypeTree.ty

	val removeTypeRefs :      TypeTree.ty -> TypeTree.ty
(*	val isReal :              TypeTree.intKind -> bool
	val isString :            TypeTree.ty -> bool *)

	val getCType :            TypeTree.ty -> string
	val getAliceType :        TypeTree.ty -> string
	val getAliceUnsafeType :  TypeTree.ty -> string

	val isOutArg :            TypeTree.ty -> bool
	val splitArgTypes :       TypeTree.ty list -> arginfo list
	val splitInOuts :         arginfo list * bool -> 
	                                arginfo list * arginfo list
        val numIns :              arginfo list * bool -> int
        val numOuts :             arginfo list * bool -> int

	val getCFunType :         string * TypeTree.ty * arginfo list * bool 
 	                               -> string

	val isItemOfSpace :       Util.spaces -> TypeTree.decl -> bool
	val checkItem :           TypeTree.decl -> bool

	val getClassList :        TypeTree.tree -> TypeTree.tree
   end
