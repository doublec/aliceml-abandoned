signature PREBOUND =
  sig

    type stamp  = IntermediateGrammar.stamp
    type id     = IntermediateGrammar.id
    type longid = IntermediateGrammar.longid


    val stamp_false :	stamp
    val stamp_true :	stamp
    val stamp_nil :	stamp
    val stamp_cons :	stamp
    val stamp_ref :	stamp
    val stamp_Match :	stamp
    val stamp_Bind :	stamp
    val stamp_eq :	stamp
    val stamp_assign :	stamp

    val id_false :	id
    val id_true :	id
    val id_nil :	id
    val id_cons :	id
    val id_ref :	id
    val id_Match :	id
    val id_Bind :	id
    val id_eq :		id
    val id_assign :	id

    val longid_false :	longid
    val longid_true :	longid
    val longid_nil :	longid
    val longid_cons :	longid
    val longid_ref :	longid
    val longid_Match :	longid
    val longid_Bind :	longid
    val longid_eq :	longid
    val longid_assign :	longid

  end
