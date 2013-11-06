(* Strengthening and where *)

signature S0 =
  sig
    type t
  end

signature S1 =
  sig
    structure M0 : S0
  end

signature S2 =
  sig
    structure M0' : S0
    structure M1 : S1 where M0 = M0'
  end
