structure FrontendSMLInitialContext : INITIAL_CONTEXT =
  struct
    type t = BindEnv.t

    fun initial() = BindEnv.clone BindEnv0.E0
  end
