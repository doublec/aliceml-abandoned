signature PREBOUND =
  sig

    type name   = Name.t
    type id     = IntermediateGrammar.id
    type longid = IntermediateGrammar.longid


    val name_false :	name
    val name_true :	name
    val name_nil :	name
    val name_cons :	name
    val name_ref :	name
    val name_Match :	name
    val name_Bind :	name
    val name_eq :	name
    val name_assign :	name

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
