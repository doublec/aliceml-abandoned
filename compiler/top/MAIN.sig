signature MAIN =
  sig

    val parseString :		string -> PostParseGrammar_Program.Program
    val parseFile :		string -> PostParseGrammar_Program.Program

    val translateString :	string -> PostTranslationIntermediate.dec list
    val translateFile :		string -> PostTranslationIntermediate.dec list

    val ozifyString :		string * string -> unit
    val ozifyFile :		string * string -> unit

    val ozifyStringToStream :	string * TextIO.outstream -> unit
    val ozifyFileToStream :	string * TextIO.outstream -> unit

  end
