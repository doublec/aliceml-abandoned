signature MAIN =
  sig

    structure Composer: COMPOSER'
    structure Switches: SWITCHES

    val compileSign :		string -> Composer.Sig.t
    val compile :		string * string * string -> Composer.Sig.t

    (*DEBUG*)
    val parseString :		string -> InputGrammar.t
    val parseFile :		string -> InputGrammar.t

    val abstractString :	string -> AbstractGrammar.t
    val abstractFile :		string -> AbstractGrammar.t

    val elabString :		string -> TypedGrammar.t
    val elabFile :		string -> TypedGrammar.t

    val translateString :	string -> IntermediateGrammar.t
    val translateFile :		string -> IntermediateGrammar.t

    val flattenString :		string -> FlatGrammar.t
    val flattenFile :		string -> FlatGrammar.t

  end
