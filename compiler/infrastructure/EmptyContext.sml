structure EmptyContext :> CONTEXT where type t = unit =
  struct
    type t = unit

    fun initial() = ()
    fun clone()   = ()
  end
