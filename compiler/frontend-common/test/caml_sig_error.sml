__prebound P

type int  = P.int
type unit = {}

signature FOO =
  sig
    type bar
    structure A : sig val fresh : unit -> bar end
  end

structure Foo :> FOO =
  struct
    structure A = struct type t = {foo:int} fun fresh() = {foo=1} end
    type bar = A.t
  end
