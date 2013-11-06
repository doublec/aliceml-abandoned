structure M =
  struct
    datatype t = T
  end

signature S =
  sig
    structure X = M
    val x : M.t
  end

structure N :> S =
  struct
    structure X = M
    val x = X.T
  end

val M.T = N.x
