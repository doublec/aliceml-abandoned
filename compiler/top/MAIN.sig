signature MAIN =
  sig

    val parseString :		string -> PostParseGrammar_Program.Program
    val translateString :	string -> PostTranslationIntermediate.dec list

    val parseFile :		string -> PostParseGrammar_Program.Program
    val translateFile :		string -> PostTranslationIntermediate.dec list

  end
