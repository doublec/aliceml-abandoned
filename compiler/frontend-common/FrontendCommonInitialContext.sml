structure FrontendCommonInitialContext : INITIAL_CONTEXT =
  struct
    type t = Env.t

    fun initial() = Env.clone Env0.E0
  end
