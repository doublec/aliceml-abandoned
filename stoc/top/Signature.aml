structure Signature :> SIGNATURE where type t = Inf.sign =
  struct
    type t = Inf.sign

    fun matches(s1,s2) =
	let
	    val j1 = Inf.inSig s1
	    val j2 = Inf.instance(Inf.inSig s2)
	in
	    ( Inf.match(j1,j2) ; true ) handle Inf.Mismatch _ => false
	end

  end
