signature TRANSLATION_PHASE =
  sig
    structure C : CONTEXT              = EmptyContext
    structure I : ABSTRACT_GRAMMAR     = TypedGrammar
    structure O : INTERMEDIATE_GRAMMAR = IntermediateGrammar

    val translate : C.t -> Source.desc * I.comp -> O.comp
  end
