signature ELAB =
  sig

    structure I : ABSTRACT_GRAMMAR = AbstractGrammar
    structure O : ABSTRACT_GRAMMAR = TypedGrammar

    type env = Env.t

    val elabProgram :	env * I.program -> O.program

  end
