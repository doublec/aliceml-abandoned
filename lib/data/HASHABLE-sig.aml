signature HASH_KEY =
  sig
    type t
    val equals :  t * t -> bool
    val hash :    t -> int
  end

signature EQ_HASH_KEY =
  sig
    eqtype t
    val hash :    t -> int
  end

functor FromEqHashKey(EqHashKey: EQ_HASH_KEY) : HASH_KEY =
  struct
    type t	= EqHashKey.t
    val equals	= op=
    val hash	= EqHashKey.hash
  end
