signature BIND_ENV_FROM_SIG =
  sig
    val envFromSig :	Source.region * Inf.sign -> BindEnv.t
  end
