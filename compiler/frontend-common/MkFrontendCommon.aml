functor MakeFrontendCommon(
		Composer: COMPOSER where type Sig.t = Inf.sign
	) : PHASE =
     ComposePhases(
	structure Phase1  = MakeElaborationPhase(Composer)
	structure Phase2  = TranslationPhase
	structure Context = Env
	fun context1 E    = E
	fun context2 E    = ()
     )
