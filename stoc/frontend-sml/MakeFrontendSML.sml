functor MakeFrontendSML(
		Composer: COMPOSER where type Sig.t = Inf.sign
	) : PHASE =
     ComposePhases(
	structure Phase1  = ParsingPhase
	structure Phase2  = MakeAbstractionPhase(Composer)
	structure Context = BindEnv
	fun context1 E    = ()
	fun context2 E    = E
     )
