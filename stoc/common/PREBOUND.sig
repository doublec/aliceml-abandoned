signature PREBOUND =
  sig

    type stamp = Stamp.t

    val name_false :	string
    val name_true :	string
    val name_nil :	string
    val name_cons :	string
    val name_ref :	string
    val name_Match :	string
    val name_Bind :	string
    val name_eq :	string
    val name_assign :	string

    val stamp_Prebound:	stamp
    val stamp_false :	stamp
    val stamp_true :	stamp
    val stamp_nil :	stamp
    val stamp_cons :	stamp
    val stamp_ref :	stamp
    val stamp_Match :	stamp
    val stamp_Bind :	stamp
    val stamp_eq :	stamp
    val stamp_assign :	stamp

    val nameToStamp :	string -> stamp

  end
