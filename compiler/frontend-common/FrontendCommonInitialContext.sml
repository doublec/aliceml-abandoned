structure FrontendCommonInitialContext : INITIAL_CONTEXT =
  struct
    type t = Env.t
    val initial = Env.new
  end
