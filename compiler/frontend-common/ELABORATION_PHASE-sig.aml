signature ELABORATION_PHASE =
  sig

    structure I : ABSTRACT_GRAMMAR = AbstractGrammar
    structure O : ABSTRACT_GRAMMAR = TypedGrammar

    type env = Env.t

    val elab :	env -> I.component -> O.component

  end
