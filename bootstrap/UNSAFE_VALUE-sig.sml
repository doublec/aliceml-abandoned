(* Dummy replacement for bootstrapping *)

signature UNSAFE_VALUE =
    sig
	val same: 'a * 'a -> bool
    end
