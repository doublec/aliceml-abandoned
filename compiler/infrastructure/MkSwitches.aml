functor MakeSwitches() :> SWITCHES =
  struct

    structure Bootstrap =
      struct
	datatype rtt_level = NO_RTT | CORE_RTT | FULL_RTT

	val implicitImport				= ref true
	val rttLevel					= ref NO_RTT
      end

    structure Debug =
      struct
	val outputPhase					= ref true
	val outputAbstractionResult			= ref false
	val outputElaborationResult			= ref false
	val outputElaborationSig			= ref true
	val outputIntermediate				= ref false
	val checkIntermediate				= ref false
	val outputFlatteningResult			= ref false
	val outputValuePropagationResult		= ref false
	val outputLivenessAnalysisIntermediate		= ref false
	val outputLivenessAnalysisResult		= ref false
	val outputTarget				= ref false
      end

    (* Backward compatibility... *)

    datatype rtt_level		= datatype Bootstrap.rtt_level

    val implicitImport		= Bootstrap.implicitImport
    val printComponentSig	= Debug.outputComponentSig
    val checkIntermediate	= Debug.checkIntermediate
    val outputAssembly		= Debug.outputTarget
    val rttLevel		= Bootstrap.rttLevel
  end
