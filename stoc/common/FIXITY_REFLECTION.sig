signature FIXITY_REFLECTION =
  sig

    val typ_assoc :	Type.t
    val typ_fix :	Type.t

    val modname_fixity:	Name.t

    val typname_assoc :	Name.t
    val lab_left :	Label.t
    val lab_right :	Label.t
    val lab_neither :	Label.t

    val typname_fix :	Name.t
    val lab_nonfix :	Label.t
    val lab_prefix :	Label.t
    val lab_postfix :	Label.t
    val lab_infix :	Label.t

  end
