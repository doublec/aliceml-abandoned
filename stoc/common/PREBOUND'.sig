signature PREBOUND' =
  sig
    type stamp  = IntermediateGrammar.stamp
    type id     = IntermediateGrammar.id
    type longid = IntermediateGrammar.longid

    val stamp_less :	stamp
    val stamp_plus :	stamp
    val stamp_times :	stamp

    val id_less :	id
    val id_plus :	id
    val id_times :	id

    val longid_less :	longid
    val longid_plus :	longid
    val longid_times :	longid
  end
