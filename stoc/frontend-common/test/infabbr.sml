__prebound P

type int = P.int

signature S =
  sig
    type t = int
    val x : t
  end

structure S =
  struct
    type t = int
    val x = 0
  end

structure S' : S = S
structure S'' :> S = S

signature T =
  sig
    structure S1 : S
    structure S2 : S = S
    structure S3 : S = S
    type t0 = int
    type t  = S.t
    type t' = S'.t
    type t'' = S''.t
    type t1 = S1.t
    type t2 = S2.t
    type t3 = S3.t
  end
