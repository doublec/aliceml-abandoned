functor MakeSwitches() :> SWITCHES =
  struct

    structure Bootstrap =
      struct
	datatype rtt_level = NO_RTT | CORE_RTT | FULL_RTT

	val implicitImport			= ref true
	val rttLevel				= ref NO_RTT
      end

    structure Debug =
      struct
	val dumpPhase				= ref true
	val dumpAbstractionResult		= ref false
	val dumpElaborationResult		= ref false
	val dumpElaborationSig			= ref true
	val dumpIntermediate			= ref false
	val checkIntermediate			= ref false
	val dumpFlatteningResult		= ref false
	val dumpValuePropagationResult		= ref false
	val dumpLivenessAnalysisIntermediate	= ref false
	val dumpLivenessAnalysisResult		= ref false
	val dumpTarget				= ref false
      end

    (* Backward compatibility... *)

    val implicitImport		= Bootstrap.implicitImport
    val outputAssembly		= Debug.dumpTarget
  end
