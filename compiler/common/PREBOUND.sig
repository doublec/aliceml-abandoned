signature PREBOUND =
  sig

    type stamp  = IntermediateGrammar.stamp

    val stamp_false :	stamp
    val stamp_true :	stamp
    val stamp_nil :	stamp
    val stamp_cons :	stamp
    val stamp_ref :	stamp
    val stamp_Match :	stamp
    val stamp_Bind :	stamp
    val stamp_eq :	stamp
    val stamp_assign :	stamp

  end
