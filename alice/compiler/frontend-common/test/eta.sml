structure M =
  struct
    datatype ('a,'b) t = T
  end

signature S =
  sig
    type ('a,'b) t = ('a,'b) M.t
  end

structure M' :> S = M
