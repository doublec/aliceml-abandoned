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

    val con_word	= (STAR, CLOSED, path_word)
    val con_int		= (STAR, CLOSED, path_int)
    val con_char	= (STAR, CLOSED, path_char)
    val con_string	= (STAR, CLOSED, path_string)
    val con_real	= (STAR, CLOSED, path_real)
    val con_bool	= (STAR, CLOSED, path_bool)
    val con_exn		= (STAR, CLOSED, path_exn)
    val con_ref		= (ARROW(STAR,STAR), CLOSED, path_ref)
    val con_vec		= (ARROW(STAR,STAR), CLOSED, path_vec)
    val con_list	= (ARROW(STAR,STAR), CLOSED, path_list)

    val typ_unit	= Type.inRow(Type.emptyRow())
    val typ_int		= Type.inCon con_int
    val typ_word	= Type.inCon con_word
    val typ_char	= Type.inCon con_char
    val typ_string	= Type.inCon con_string
    val typ_real	= Type.inCon con_real
    val typ_bool	= Type.inCon con_bool
    val typ_exn		= Type.inCon con_exn
    val typ_ref		= Type.inCon con_ref
    val typ_vec		= Type.inCon con_vec
    val typ_list	= Type.inCon con_list

  end
