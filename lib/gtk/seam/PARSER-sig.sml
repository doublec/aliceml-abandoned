signature PARSER =
    sig
	exception EParseError
	val parse : string -> TypeTree.tree
    end
