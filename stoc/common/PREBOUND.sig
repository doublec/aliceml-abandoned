signature PREBOUND =
  sig

    type name  = Name.t
    type stamp = Stamp.t
    type path  = Path.t

    val valname_false :		name
    val valname_true :		name
    val valname_nil :		name
    val valname_cons :		name
    val valname_ref :		name
    val valname_match :		name
    val valname_bind :		name

    val typname_bool :		name
    val typname_int :		name
    val typname_word :		name
    val typname_real :		name
    val typname_string :	name
    val typname_char :		name
    val typname_list :		name
    val typname_vec :		name
    val typname_ref :		name
    val typname_exn :		name

    val valstamp_false :	stamp
    val valstamp_true :		stamp
    val valstamp_nil :		stamp
    val valstamp_cons :		stamp
    val valstamp_ref :		stamp
    val valstamp_match :	stamp
    val valstamp_bind :		stamp

    val typstamp_bool :		stamp
    val typstamp_int :		stamp
    val typstamp_word :		stamp
    val typstamp_real :	stamp
    val typstamp_string :	stamp
    val typstamp_char :		stamp
    val typstamp_list :		stamp
    val typstamp_vec :		stamp
    val typstamp_ref :		stamp
    val typstamp_exn :		stamp

    val valpath_false :		path
    val valpath_true :		path
    val valpath_nil :		path
    val valpath_cons :		path
    val valpath_ref :		path
    val valpath_match :		path
    val valpath_bind :		path

    val typpath_bool :		path
    val typpath_int :		path
    val typpath_word :		path
    val typpath_real :		path
    val typpath_string :	path
    val typpath_char :		path
    val typpath_list :		path
    val typpath_vec :		path
    val typpath_ref :		path
    val typpath_exn :		path

  end
