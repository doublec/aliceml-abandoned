signature MAIN =
  sig

    val parseString :		string -> InputGrammar.Program
    val parseFile :		string -> InputGrammar.Program

    val abstractString :	string -> AbstractGrammar.program
    val abstractFile :		string -> AbstractGrammar.program

    val translateString :	string -> IntermediateGrammar.program
    val translateFile :		string -> IntermediateGrammar.program

    val simplifyString :	string -> SimplifiedGrammar.program
    val simplifyFile :		string -> SimplifiedGrammar.program

    val imperatifyString :	string -> ImperativeGrammar.program
    val imperatifyFile :	string -> ImperativeGrammar.program

    val ozifyString :		string * TextIO.outstream -> unit
    val ozifyFile :		string * TextIO.outstream -> unit

    val ozifyStringToStdOut :	string -> unit
    val ozifyFileToStdOut :	string -> unit

    val ozifyStringToFile :	string * string -> unit
    val ozifyFileToFile :	string * string -> unit

  end
