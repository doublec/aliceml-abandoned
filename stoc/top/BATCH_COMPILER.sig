signature MAIN =
  sig

    structure Switches: SWITCHES

    val parseString :		string -> InputGrammar.t
    val parseFile :		string -> InputGrammar.t

    val abstractString :	string -> AbstractGrammar.t
    val abstractFile :		string -> AbstractGrammar.t

    val elabString :		string -> TypedGrammar.t
    val elabFile :		string -> TypedGrammar.t

    val translateString :	string -> IntermediateGrammar.t
    val translateFile :		string -> IntermediateGrammar.t

    val flattenString :		string -> FlatGrammar.component
    val flattenFile :		string -> FlatGrammar.component

    val compileForMozart :	string * string -> IntermediateGrammar.sign

  end
