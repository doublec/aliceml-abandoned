functor MakeDumpingPhase(
	structure Phase :    PHASE
	structure Switches : SWITCHES
	val header : string
	val pp :     Phase.O.t -> PrettyPrint.doc
	val switch : bool ref
    ) : PHASE =
struct
    open Phase

    fun translate context desc_rep =
	let
	    val rep' = Phase.translate context desc_rep
	in
	    if not(!switch) then () else
	    (
		TextIO.output(Switches.Debug.logOut, "-- " ^ header ^ ":\n");
		PrettyPrint.output(Switches.Debug.logOut, pp rep',
				   !Switches.Debug.logWidth);
		TextIO.output(Switches.Debug.logOut, "\n")
	    );
	    rep'
	end
end
