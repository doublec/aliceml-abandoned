signature PREBOUND_TYPE =
  sig

    type path = Path.t
    type con  = Type.con
    type typ  = Type.typ

    val path_bool :		path (* = Prebound.typpath_bool *)
    val path_int :		path (* = Prebound.typpath_int *)
    val path_word :		path (* = Prebound.typpath_word *)
    val path_real :		path (* = Prebound.typpath_real *)
    val path_string :		path (* = Prebound.typpath_string *)
    val path_char :		path (* = Prebound.typpath_char *)
    val path_list :		path (* = Prebound.typpath_list *)
    val path_vec :		path (* = Prebound.typpath_vec *)
    val path_ref :		path (* = Prebound.typpath_ref *)
    val path_exn :		path (* = Prebound.typpath_exn *)

    val con_bool :		con
    val con_int :		con
    val con_word :		con
    val con_real :		con
    val con_string :		con
    val con_char :		con
    val con_list :		con
    val con_vec :		con
    val con_ref :		con
    val con_exn :		con

    val typ_unit :		typ
    val typ_bool :		typ
    val typ_int :		typ
    val typ_word :		typ
    val typ_real :		typ
    val typ_string :		typ
    val typ_char :		typ
    val typ_list :		typ
    val typ_vec :		typ
    val typ_ref :		typ
    val typ_exn :		typ

  end
