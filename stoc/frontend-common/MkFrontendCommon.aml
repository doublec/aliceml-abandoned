functor MakeFrontendCommon(
		val loadSign: Source.desc * Url.t -> Inf.sign
		structure Switches: SWITCHES
	) : PHASE =
    let
	structure Phase1 =
		  MakeTracingPhase(
			structure Phase    =
			    MakeElaborationPhase(val loadSign = loadSign)
			structure Switches = Switches
			val name = "Elaboration"
		  )
	structure Phase2 =
		  MakeTracingPhase(
			structure Phase    = MakeTranslationPhase(Switches)
			structure Switches = Switches
			val name = "Translation"
		  )
	structure Phase1' =
		  MakeDumpingPhase(
			structure Phase    = Phase1
			structure Switches = Switches
			val header = "Component Signature"
			val pp     = PPInf.ppSig o #sign o TypedGrammar.infoComp
			val switch = Switches.Debug.dumpElaborationSig
		  )
	structure Phase2' =
		  MakeDumpingPhase(
			structure Phase    = Phase2
			structure Switches = Switches
			val header = "Intermediate Syntax"
			val pp     = PPIntermediateGrammar.ppComp
			val switch = Switches.Debug.dumpIntermediate
		  )
    in
	ComposePhases(
	    structure Phase1  = Phase1'
	    structure Phase2  = Phase2'
	    structure Context = Env
	    fun context1 E    = E
	    fun context2 E    = ()
	)
    end
