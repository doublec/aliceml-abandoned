functor MakeFrontendSML(
		structure Composer: COMPOSER where type Sig.t = Inf.sign
		structure Switches: SWITCHES
	) : PHASE =
    let
	structure Phase2 =
		  MakeDumpingPhase(
			structure Phase    = MakeAbstractionPhase(Composer)
			structure Switches = Switches
			val header = "Abstract Syntax"
			val pp     = PPAbstractGrammar.ppComp
			val switch = Switches.Debug.dumpAbstractionResult
		  )
    in
	ComposePhases(
	    structure Phase1  = ParsingPhase
	    structure Phase2  = Phase2
	    structure Context = BindEnv
	    fun context1 E    = ()
	    fun context2 E    = E
	)
    end
