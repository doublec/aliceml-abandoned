structure Prebound :> PREBOUND =
  struct

    type stamp = Stamp.t

    val stamp_false	= Stamp.new()
    val stamp_true	= Stamp.new()
    val stamp_nil	= Stamp.new()
    val stamp_cons	= Stamp.new()
    val stamp_ref	= Stamp.new()
    val stamp_Match	= Stamp.new()
    val stamp_Bind	= Stamp.new()

    val stamp_bool	= Stamp.new()
    val stamp_int	= Stamp.new()
    val stamp_word	= Stamp.new()
    val stamp_real	= Stamp.new()
    val stamp_string	= Stamp.new()
    val stamp_char	= Stamp.new()
    val stamp_list	= Stamp.new()
    val stamp_vec	= Stamp.new()
    val stamp_exn	= Stamp.new()

  end
