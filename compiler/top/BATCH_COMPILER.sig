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

    val ozifyString :		string * string -> unit
    val ozifyFile :		string * string -> unit

    val ozifyStringToStream :	string * TextIO.outstream -> unit
    val ozifyFileToStream :	string * TextIO.outstream -> unit

  end
