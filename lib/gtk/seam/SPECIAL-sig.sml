
signature SPECIAL =
    sig
	val includeFiles :   string list
	val ignoreFuns :     string list
        val specialFuns :    TypeTree.decl list
        val changedFuns :    TypeTree.decl list
        val ignoreSafeFuns : string list


        val isIgnored :      TypeTree.decl -> bool
        val isIgnoredSafe :  TypeTree.decl -> bool
    end
