structure PreboundType :> PREBOUND_TYPE =
  struct

    type path		= Path.t
    type con		= Type.con
    type typ		= Type.typ

    datatype kind	= datatype Type.kind
    datatype sort	= datatype Type.sort

    val path_bool	= Prebound.typpath_bool
    val path_int	= Prebound.typpath_int
    val path_word	= Prebound.typpath_word
    val path_real	= Prebound.typpath_real
    val path_string	= Prebound.typpath_string
    val path_char	= Prebound.typpath_char
    val path_list	= Prebound.typpath_list
    val path_vec	= Prebound.typpath_vec
    val path_ref	= Prebound.typpath_ref
    val path_exn	= Prebound.typpath_exn

    val con_bool	= (STAR, CLOSED, path_bool)
    val con_word	= (STAR, CLOSED, path_word)
    val con_int		= (STAR, CLOSED, path_int)
    val con_char	= (STAR, CLOSED, path_char)
    val con_string	= (STAR, CLOSED, path_string)
    val con_real	= (STAR, CLOSED, path_real)
    val con_exn		= (STAR, CLOSED, path_exn)
    val con_list	= (ARROW(STAR,STAR), CLOSED, path_list)
    val con_vec		= (ARROW(STAR,STAR), CLOSED, path_vec)
    val con_ref		= (ARROW(STAR,STAR), CLOSED, path_ref)

    val lab_false	= Label.fromName(Prebound.valname_false)
    val lab_true	= Label.fromName(Prebound.valname_true)
    val lab_nil		= Label.fromName(Prebound.valname_nil)
    val lab_cons	= Label.fromName(Prebound.valname_cons)

    val typ_list	= Type.unknown(ARROW(STAR,STAR))
    val var_list	= Type.var STAR
    val typ_var		= Type.inVar var_list
    val typ_cons	= Type.inTuple [typ_var, Type.inApply(typ_list,typ_var)]

    val row_unit	= Type.emptyRow()
    val row_bool	= Type.extendRow(lab_false, [],
			  Type.extendRow(lab_true, [], Type.emptyRow()))
    val row_list	= Type.extendRow(lab_cons, [typ_cons],
			  Type.extendRow(lab_nil, [], Type.emptyRow()))

    val typ_unit	= Type.inProd row_unit
    val typ_int		= Type.inCon con_int
    val typ_word	= Type.inCon con_word
    val typ_char	= Type.inCon con_char
    val typ_string	= Type.inCon con_string
    val typ_real	= Type.inCon con_real
    val typ_bool	= Type.inAbbrev(Type.inCon con_bool,
					Type.inMu(Type.inSum row_bool))
    val typ_exn		= Type.inCon con_exn
    val typ_ref		= Type.inCon con_ref
    val typ_vec		= Type.inCon con_vec
    val _		= Type.unify(typ_list,
			    Type.inAbbrev(Type.inCon con_list,
				Type.inMu(Type.inLambda(var_list,
							Type.inSum row_list))))
  end
