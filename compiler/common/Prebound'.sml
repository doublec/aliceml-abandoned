structure Prebound' :> PREBOUND' =
  struct
    type name   = IntermediateGrammar.stamp
    type id     = IntermediateGrammar.id
    type longid = IntermediateGrammar.longid

    open IntermediateGrammar

    val dummy = Source.nowhere

    val stamp_less	= Stamp.new()
    val stamp_plus	= Stamp.new()
    val stamp_times	= Stamp.new()

    val id_less		= Id(dummy, stamp_less,  ExId "<")
    val id_plus		= Id(dummy, stamp_plus,  ExId "+")
    val id_times	= Id(dummy, stamp_times, ExId "*")

    val longid_less	= ShortId(dummy, id_less)
    val longid_plus	= ShortId(dummy, id_plus)
    val longid_times	= ShortId(dummy, id_times)
  end
