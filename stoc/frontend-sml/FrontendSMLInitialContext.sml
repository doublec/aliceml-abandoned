structure FrontendSMLInitialContext : INITIAL_CONTEXT =
  struct
    type t = BindEnv.t
    val initial = BindEnv.new
  end
