signature ABSTRACTION_PHASE =
  sig

    structure I : INPUT_GRAMMAR    = InputGrammar
    structure O : ABSTRACT_GRAMMAR = AbstractGrammar

    type Env = BindEnv.Env

    val translate :	Env -> I.Component -> O.component

  end
