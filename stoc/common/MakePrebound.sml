functor MakePrebound(val valid_false :	string
		     val valid_true :	string
		     val valid_nil :	string
		     val valid_cons :	string
		     val valid_ref :	string
		     val valid_match :	string
		     val valid_bind :	string
		     val typid_bool :	string
		     val typid_int :	string
		     val typid_word :	string
		     val typid_real :	string
		     val typid_string :	string
		     val typid_char :	string
		     val typid_list :	string
		     val typid_vec :	string
		     val typid_ref :	string
		     val typid_exn :	string
		    ) :> PREBOUND =
  struct

    type name  = Name.t
    type stamp = Stamp.t
    type path  = Path.t

    val valname_false	= Name.ExId valid_false
    val valname_true	= Name.ExId valid_true
    val valname_nil	= Name.ExId valid_nil
    val valname_cons	= Name.ExId valid_cons
    val valname_ref	= Name.ExId valid_ref
    val valname_match	= Name.ExId valid_match
    val valname_bind	= Name.ExId valid_bind

    val typname_bool	= Name.ExId typid_bool
    val typname_int	= Name.ExId typid_int
    val typname_word	= Name.ExId typid_word
    val typname_real	= Name.ExId typid_real
    val typname_string	= Name.ExId typid_string
    val typname_char	= Name.ExId typid_char
    val typname_list	= Name.ExId typid_list
    val typname_vec	= Name.ExId typid_vec
    val typname_ref	= Name.ExId typid_ref
    val typname_exn	= Name.ExId typid_exn

    val valstamp_false	= Stamp.new()
    val valstamp_true	= Stamp.new()
    val valstamp_nil	= Stamp.new()
    val valstamp_cons	= Stamp.new()
    val valstamp_ref	= Stamp.new()
    val valstamp_match	= Stamp.new()
    val valstamp_bind	= Stamp.new()

    val typstamp_bool	= Stamp.new()
    val typstamp_int	= Stamp.new()
    val typstamp_word	= Stamp.new()
    val typstamp_real	= Stamp.new()
    val typstamp_string	= Stamp.new()
    val typstamp_char	= Stamp.new()
    val typstamp_list	= Stamp.new()
    val typstamp_vec	= Stamp.new()
    val typstamp_ref	= Stamp.new()
    val typstamp_exn	= Stamp.new()

    val valpath_false	= Path.global valname_false
    val valpath_true	= Path.global valname_true
    val valpath_nil	= Path.global valname_nil
    val valpath_cons	= Path.global valname_cons
    val valpath_ref	= Path.global valname_ref
    val valpath_match	= Path.global valname_match
    val valpath_bind	= Path.global valname_bind

    val typpath_int	= Path.global typname_int
    val typpath_word	= Path.global typname_word
    val typpath_char	= Path.global typname_char
    val typpath_string	= Path.global typname_string
    val typpath_real	= Path.global typname_real
    val typpath_bool	= Path.global typname_bool
    val typpath_exn	= Path.global typname_exn
    val typpath_ref	= Path.global typname_ref
    val typpath_vec	= Path.global typname_vec
    val typpath_list	= Path.global typname_list

  end
