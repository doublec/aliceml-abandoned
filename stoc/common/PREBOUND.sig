signature PREBOUND =
  sig

    type stamp = Stamp.t

    val stamp_false :	stamp
    val stamp_true :	stamp
    val stamp_nil :	stamp
    val stamp_cons :	stamp
    val stamp_ref :	stamp
    val stamp_Match :	stamp
    val stamp_Bind :	stamp

    val stamp_bool :	stamp
    val stamp_int :	stamp
    val stamp_word :	stamp
    val stamp_real :	stamp
    val stamp_string :	stamp
    val stamp_char :	stamp
    val stamp_list :	stamp
    val stamp_exn :	stamp

  end
