functor MakePervasiveType(val labid_false :	string
			  val labid_true :	string
			  val labid_nil :	string
			  val labid_cons :	string
			  val typid_bool :	string
			  val typid_list :	string
			  val typid_int :	string
			  val typid_word :	string
			  val typid_real :	string
			  val typid_char :	string
			  val typid_string :	string
			  val typid_vec :	string
			  val typid_array :	string
			  val typid_ref :	string
			  val typid_exn :	string
			  val typid_time :	string
			  val typid_prom :	string
			  val valid_match :	string
			  val valid_bind :	string
			 ) :> PERVASIVE_TYPE =
  struct

    type lab		= Label.t
    type name		= Name.t
    type path		= Path.t
    type con		= Type.con
    type typ		= Type.typ

    datatype kind	= datatype Type.kind
    datatype sort	= datatype Type.sort

    val stamp_pervasive	= Stamp.new()

    val lab_false	= Label.fromString labid_false
    val lab_true	= Label.fromString labid_true
    val lab_nil		= Label.fromString labid_nil
    val lab_cons	= Label.fromString labid_cons

    val name_bool	= Name.ExId typid_bool
    val name_list	= Name.ExId typid_list
    val name_int	= Name.ExId typid_int
    val name_word	= Name.ExId typid_word
    val name_real	= Name.ExId typid_real
    val name_char	= Name.ExId typid_char
    val name_string	= Name.ExId typid_string
    val name_vec	= Name.ExId typid_vec
    val name_array	= Name.ExId typid_array
    val name_ref	= Name.ExId typid_ref
    val name_exn	= Name.ExId typid_exn
    val name_time	= Name.ExId typid_time
    val name_prom	= Name.ExId typid_prom

    val name_match	= Name.ExId valid_match
    val name_bind	= Name.ExId valid_bind

    val path_bool	= Path.pervasive typid_bool
    val path_list	= Path.pervasive typid_list
    val path_int	= Path.pervasive typid_int
    val path_word	= Path.pervasive typid_word
    val path_real	= Path.pervasive typid_real
    val path_char	= Path.pervasive typid_char
    val path_string	= Path.pervasive typid_string
    val path_vec	= Path.pervasive typid_vec
    val path_array	= Path.pervasive typid_array
    val path_ref	= Path.pervasive typid_ref
    val path_exn	= Path.pervasive typid_exn
    val path_time	= Path.pervasive typid_time
    val path_prom	= Path.pervasive typid_prom

    val con_bool	= (STAR, CLOSED, path_bool)
    val con_list	= (ARROW(STAR,STAR), CLOSED, path_list)
    val con_int		= (STAR, CLOSED, path_int)
    val con_word	= (STAR, CLOSED, path_word)
    val con_real	= (STAR, CLOSED, path_real)
    val con_char	= (STAR, CLOSED, path_char)
    val con_string	= (STAR, CLOSED, path_string)
    val con_exn		= (STAR, CLOSED, path_exn)
    val con_vec		= (ARROW(STAR,STAR), CLOSED, path_vec)
    val con_array	= (ARROW(STAR,STAR), CLOSED, path_array)
    val con_ref		= (ARROW(STAR,STAR), CLOSED, path_ref)
    val con_time	= (STAR, CLOSED, path_time)
    val con_prom	= (ARROW(STAR,STAR), CLOSED, path_prom)

    val typ_list	= Type.unknown(ARROW(STAR,STAR))
    val var_list	= Type.var STAR
    val typ_var		= Type.inVar var_list
    val typ_cons	= Type.inTuple #[typ_var,Type.inApply(typ_list,typ_var)]

    val row_bool	= Type.extendRow(lab_false, #[],
			  Type.extendRow(lab_true, #[], Type.emptyRow()))
    val row_list	= Type.extendRow(lab_cons, #[typ_cons],
			  Type.extendRow(lab_nil, #[], Type.emptyRow()))

    val typ_unit	= Type.inTuple #[]
    val typ_bool	= Type.inAbbrev(Type.inCon con_bool,
					Type.inMu(Type.inSum row_bool))
    val typ_int		= Type.inCon con_int
    val typ_word	= Type.inCon con_word
    val typ_real	= Type.inCon con_real
    val typ_char	= Type.inCon con_char
    val typ_string	= Type.inCon con_string
    val typ_vec		= Type.inCon con_vec
    val typ_array	= Type.inCon con_array
    val typ_ref		= Type.inCon con_ref
    val typ_exn		= Type.inCon con_exn
    val typ_prom	= Type.inCon con_prom
    val typ_time	= Type.inCon con_time
    val _		= Type.fill(typ_list,
			    Type.inAbbrev(Type.inCon con_list,
				Type.inMu(Type.inLambda(var_list,
							Type.inSum row_list))))

    exception Lookup

    fun lookup s =
	if      s = typid_int    then con_int
	else if s = typid_word   then con_word
	else if s = typid_real   then con_real
	else if s = typid_char   then con_char
	else if s = typid_string then con_string
	else if s = typid_vec	 then con_vec
	else if s = typid_array  then con_array
	else if s = typid_ref    then con_ref
	else if s = typid_exn    then con_exn
	else if s = typid_time   then con_time
	else if s = typid_prom   then con_prom
	else raise Lookup

  end
