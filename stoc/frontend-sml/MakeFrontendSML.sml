functor MakeFrontendSML(
		val loadSign: Source.desc * Url.t -> Inf.sign
		structure Switches: SWITCHES
	) : PHASE =
    let
	structure Phase1 =
		  MakeTracingPhase(
			structure Phase    = MakeParsingPhase(Switches)
			structure Switches = Switches
			val name = "Parsing"
		  )
	structure Phase2 =
		  MakeTracingPhase(
			structure Phase    =
			    MakeAbstractionPhase(val loadSign = loadSign)
			structure Switches = Switches
			val name = "Abstraction"
		  )
	structure Phase2' =
		  MakeDumpingPhase(
			structure Phase    = Phase2
			structure Switches = Switches
			val header = "Abstract Syntax"
			val pp     = PPAbstractGrammar.ppComp
			val switch = Switches.Debug.dumpAbstractionResult
		  )
    in
	ComposePhases(
	    structure Phase1  = Phase1
	    structure Phase2  = Phase2'
	    structure Context = BindEnv
	    fun context1 E    = ()
	    fun context2 E    = E
	)
    end
