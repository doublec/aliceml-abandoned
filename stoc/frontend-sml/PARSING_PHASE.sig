signature PARSE =
  sig

    (* Import *)

    structure Grammar: GRAMMAR_PROGRAM

    type source  = Source.source
    type Program = Grammar.Program


    (* Export *)

    val parse: source -> Program

  end
