signature MAIN =
  sig

    val parseString: string -> unit		(* Parse only *)
(*  val elabString:  string -> unit		(* Parse and elaborate *)
    val evalString:  string -> unit		(* Parse, elaborate, and evaluate *)
*)
    val parseFile:   string -> unit
(*  val elabFile:    string -> unit
    val evalFile:    string -> unit

    val parseFiles:  string -> unit
    val elabFiles:   string -> unit
    val evalFiles:   string -> unit

    val session:     unit   -> unit
*)
  end
