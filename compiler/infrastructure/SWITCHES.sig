signature SWITCHES =
  sig
    datatype rtt_level = NO_RTT | CORE_RTT | FULL_RTT

    val implicitImport :	bool ref
    val printComponentSig :	bool ref
    val checkIntermediate :	bool ref
    val outputAssembly :	bool ref
    val rttLevel :		rtt_level ref

    structure Bootstrap :
      sig
	datatype rtt_level = NO_RTT | CORE_RTT | FULL_RTT

	val implicitImport :				bool ref
	val rttLevel :					rtt_level ref
      end

    structure Debug :
      sig
	val outputPhase :				bool ref
	val outputAbstractionResult :			bool ref
	val outputElaborationResult :			bool ref
	val outputElaborationSig :			bool ref
	val outputIntermediate :			bool ref
	val checkIntermediate :				bool ref
	val outputFlatteningResult :			bool ref
	val outputValuePropagationResult :		bool ref
	val outputLivenessAnalysisIntermediate :	bool ref
	val outputLivenessAnalysisResult :		bool ref
	val outputTarget :				bool ref
      end
  end
