signature TYPE_REFLECTION =
  sig

    val typ_sort :	Type.t
    val typ_kind :	Type.t
    val typ_con :	Type.t
    val typ_typ :	Type.t
    val typ_var :	Type.t
    val typ_row :	Type.t

    val modname_type :	Name.t

    val typname_sort :	Name.t
    val lab_open :	Label.t
    val lab_closed :	Label.t

    val typname_kind :	Name.t
    val lab_star :	Label.t
    val lab_arrow :	Label.t

    val typname_typ :	Name.t
    val typname_var :	Name.t
    val typname_row :	Name.t

    val lab_unknown :	Label.t
    val lab_inArrow :	Label.t
    val lab_inTuple :	Label.t
    val lab_inProd :	Label.t
    val lab_inSum :	Label.t
    val lab_inVar :	Label.t
    val lab_inCon :	Label.t
    val lab_inAll :	Label.t
    val lab_inExist :	Label.t
    val lab_inApply :	Label.t
    val lab_inLambda :	Label.t
    val lab_inMu :	Label.t

    val lab_var :	Label.t
    val lab_kind :	Label.t

    val lab_unknownRow:	Label.t
    val lab_emptyRow :	Label.t
    val lab_extendRow :	Label.t

    val lab_fill :	Label.t

    val lab_equals :	Label.t  

  end
