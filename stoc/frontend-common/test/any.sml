(*
structure M0 = struct end

signature S0 =
  sig
    structure M : any
  end

signature S1 = S0 where M = M0

functor F(X : any) = struct structure M = X end

functor G(Y : S1) = struct end

structure D = G(F(M0))
*)

type u

signature S0' =
  sig
    type t
  end

signature S1' = S0' where type t = u

functor F'(type x) = struct type t = x end

functor G'(Y : S1') = struct end

structure D' = G'(F'(type x = u))
