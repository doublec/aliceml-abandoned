signature TRANSLATION_PHASE =
  sig

    structure I : ABSTRACT_GRAMMAR     = AbstractGrammar
    structure O : INTERMEDIATE_GRAMMAR = IntermediateGrammar

    val translate :	I.program -> O.program

  end
