signature MAIN =
  sig

    val parseString :		string -> InputGrammar.Component
    val parseFile :		string -> InputGrammar.Component

    val abstractString :	string -> AbstractGrammar.component
    val abstractFile :		string -> AbstractGrammar.component

    val elabString :		string -> TypedGrammar.component
    val elabFile :		string -> TypedGrammar.component

    val translateString :	string -> IntermediateGrammar.component
    val translateFile :		string -> IntermediateGrammar.component
    val translateString' :	string -> IntermediateGrammar.component
    val translateFile' :	string -> IntermediateGrammar.component

    val imperatifyString :	string -> ImperativeGrammar.component
    val imperatifyFile :	string -> ImperativeGrammar.component
    val imperatifyString' :	string -> ImperativeGrammar.component
    val imperatifyFile' :	string -> ImperativeGrammar.component

    val ozifyStringToStdOut :	string -> unit
    val ozifyFileToStdOut :	string -> unit
    val ozifyStringToStdOut' :	string -> unit
    val ozifyFileToStdOut' :	string -> unit

    val ozifyStringToFile :	string * string -> unit
    val ozifyFileToFile :	string * string -> unit
    val ozifyStringToFile' :	string * string -> unit
    val ozifyFileToFile' :	string * string -> unit

    val debugStringToStdOut :	string -> unit
    val debugFileToStdOut :	string -> unit
    val debugStringToStdOut' :	string -> unit
    val debugFileToStdOut' :	string -> unit

    val debugStringToFile :	string * string -> unit
    val debugFileToFile :	string * string -> unit
    val debugStringToFile' :	string * string -> unit
    val debugFileToFile' :	string * string -> unit

    val comifyStringToStdOut :	string -> unit
    val comifyFileToStdOut :	string -> unit
    val comifyStringToStdOut' :	string -> unit
    val comifyFileToStdOut' :	string -> unit

    val comifyStringToFile :	string * string -> unit
    val comifyFileToFile :	string * string -> unit
    val comifyStringToFile' :	string * string -> unit
    val comifyFileToFile' :	string * string -> unit

  end
