
signature SPECIAL =
    sig
	val includeFile :   string * string * int

        val specialFuns :    TypeTree.decl list
        val changedFuns :    TypeTree.decl list

        val isIgnored :      TypeTree.decl -> bool
        val isIgnoredSafe :  TypeTree.decl -> bool
    end
