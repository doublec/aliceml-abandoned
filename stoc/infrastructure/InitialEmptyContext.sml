structure InitialEmptyContext :> INITIAL_CONTEXT where type t = unit =
  struct
    type t = unit

    fun initial() = ()
  end
