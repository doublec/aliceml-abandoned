structure Prebound :> PREBOUND =
  struct

    type name   = Name.t
    type id     = IntermediateGrammar.id
    type longid = IntermediateGrammar.longid


    open IntermediateGrammar

    val dummy = Source.nowhere

    val name_false	= Name.new "false"
    val name_true	= Name.new "true"
    val name_nil	= Name.new "nil"
    val name_cons	= Name.new "cons"
    val name_ref	= Name.new "ref"
    val name_Match	= Name.new "Match"
    val name_Bind	= Name.new "Bind"
    val name_eq		= Name.new "="
    val name_assign	= Name.new ":="

    val id_false	= Id(dummy, name_false)
    val id_true		= Id(dummy, name_true)
    val id_nil		= Id(dummy, name_nil)
    val id_cons		= Id(dummy, name_cons)
    val id_ref		= Id(dummy, name_ref)
    val id_Match	= Id(dummy, name_Match)
    val id_Bind		= Id(dummy, name_Bind)
    val id_eq		= Id(dummy, name_eq)
    val id_assign	= Id(dummy, name_assign)

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
