structure Name :> NAME =
  struct

    datatype name = ExId of string | InId

    type t = name

  end
