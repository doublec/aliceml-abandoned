structure Prebound :> PREBOUND =
  struct

    type stamp = Stamp.t

    val name_false	= "__false"
    val name_true	= "__true"
    val name_nil 	= "__nil"
    val name_cons 	= "__cons"
    val name_ref 	= "__ref"
    val name_Match	= "__Match"
    val name_Bind 	= "__Bind"
    val name_eq		= "__eq"
    val name_assign	= "__assign"

    val stamp_Prebound	= Stamp.new()
    val stamp_false	= Stamp.new()
    val stamp_true	= Stamp.new()
    val stamp_nil	= Stamp.new()
    val stamp_cons	= Stamp.new()
    val stamp_ref	= Stamp.new()
    val stamp_Match	= Stamp.new()
    val stamp_Bind	= Stamp.new()
    val stamp_eq	= Stamp.new()
    val stamp_assign	= Stamp.new()

    (* The strings here must mirror the above! *)

    fun nameToStamp "__false"	= stamp_false
      | nameToStamp "__true"	= stamp_true
      | nameToStamp "__nil"	= stamp_nil
      | nameToStamp "__cons"	= stamp_cons
      | nameToStamp "__ref"	= stamp_ref
      | nameToStamp "__Match"	= stamp_Match
      | nameToStamp "__Bind"	= stamp_Bind
      | nameToStamp "__eq"	= stamp_eq
      | nameToStamp "__assign"	= stamp_assign
      | nameToStamp _		= Crash.crash "Prebound.nameToStamp: unknown"

  end
