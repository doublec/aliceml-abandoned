signature MAIN =
  sig

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

    val ozifyStringToStdOut :	string -> unit
    val ozifyFileToStdOut :	string -> unit

    val ozifyStringToFile :	string * string -> unit
    val ozifyFileToFile :	string * string -> unit

    val debugStringToStdOut :	string -> unit
    val debugFileToStdOut :	string -> unit

    val debugStringToFile :	string * string -> unit
    val debugFileToFile :	string * string -> unit

    val comifyStringToStdOut :	string -> unit
    val comifyFileToStdOut :	string -> unit

    val comifyStringToFile :	string * string -> unit
    val comifyFileToFile :	string * string -> unit

  end
