signature PARSING_PHASE =
  sig
    structure C : CONTEXT       = EmptyContext
    structure I : SOURCE        = Source
    structure O : INPUT_GRAMMAR = InputGrammar

    val translate : C.t -> I.source -> InputGrammar.Component
  end
