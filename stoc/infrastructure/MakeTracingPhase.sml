functor MakeTracingPhase(
	structure Phase :    PHASE
	structure Switches : SWITCHES
	val name : string
    ) : PHASE =
struct
    open Phase

    fun translate context desc_rep =
	( if not(!Switches.Debug.dumpPhases) then () else
	     TextIO.output(Switches.Debug.logOut, "-- " ^ name ^ "...\n")
	; Phase.translate context desc_rep
	)
end
