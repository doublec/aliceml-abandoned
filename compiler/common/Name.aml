structure Name :> NAME =
  struct

    datatype name = ExId of string | InId
    type     t    = name

    fun toString(ExId s) = s
      | toString InId    = "?"

  end
