structure Prebound :> PREBOUND =
  struct

    type name   = IntermediateGrammar.stamp
    type id     = IntermediateGrammar.id
    type longid = IntermediateGrammar.longid


    open IntermediateGrammar

    val dummy = Source.nowhere

    val stamp_false	= Stamp.new()
    val stamp_true	= Stamp.new()
    val stamp_nil	= Stamp.new()
    val stamp_cons	= Stamp.new()
    val stamp_ref	= Stamp.new()
    val stamp_Match	= Stamp.new()
    val stamp_Bind	= Stamp.new()
    val stamp_eq	= Stamp.new()
    val stamp_assign	= Stamp.new()

    val id_false	= Id(dummy, stamp_false, ExId "false")
    val id_true		= Id(dummy, stamp_true,  ExId "true")
    val id_nil		= Id(dummy, stamp_nil,   ExId "nil")
    val id_cons		= Id(dummy, stamp_cons,  ExId "::")
    val id_ref		= Id(dummy, stamp_ref,   ExId "ref")
    val id_Match	= Id(dummy, stamp_Match, ExId "Match")
    val id_Bind		= Id(dummy, stamp_Bind,  ExId "Bind")
    val id_eq		= Id(dummy, stamp_eq,    ExId "=")
    val id_assign	= Id(dummy, stamp_assign,ExId ":=")

    val longid_false	= ShortId(dummy, id_false)
    val longid_true	= ShortId(dummy, id_true)
    val longid_nil	= ShortId(dummy, id_nil)
    val longid_cons	= ShortId(dummy, id_cons)
    val longid_ref	= ShortId(dummy, id_ref)
    val longid_Match	= ShortId(dummy, id_Match)
    val longid_Bind	= ShortId(dummy, id_Bind)
    val longid_eq	= ShortId(dummy, id_eq)
    val longid_assign	= ShortId(dummy, id_assign)

  end
