functor MakeFrontendCommon(
		structure Composer: COMPOSER where type Sig.t = Inf.sign
		structure Switches: SWITCHES
	) : PHASE =
     ComposePhases(
	structure Phase1  = MakeElaborationPhase(structure Composer = Composer
						 structure Switches = Switches)
	structure Phase2  = TranslationPhase
	structure Context = Env
	fun context1 E    = E
	fun context2 E    = ()
     )
