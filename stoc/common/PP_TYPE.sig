signature PP_TYPE =
  sig

    type doc  = PrettyPrint.doc
    type typ  = Type.typ
    type kind = Type.kind

    val ppType :	typ -> doc
    val ppKind :	kind -> doc

  end
