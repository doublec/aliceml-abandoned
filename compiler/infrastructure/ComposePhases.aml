functor ComposePhases(
	structure Phase1:  PHASE
	structure Phase2:  PHASE where I = Phase1.O
	structure Context: CONTEXT
	val context1: Context.t -> Phase1.C.t
	val context2: Context.t -> Phase2.C.t
    ) : PHASE =
  struct
    structure I = Phase1.I
    structure O = Phase2.O
    structure C = Context

    fun translate context = Phase2.translate(context2 context)
			  o Phase1.translate(context1 context)
  end
