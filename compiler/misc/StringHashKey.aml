structure StringHashKey : HASH_KEY =
  struct

    type t = string

    open Word
    infix << >> andb xorb

    fun hash s =	(* hashpjw [Aho/Sethi/Ullman "Compilers"] *)
	let
	    val n = String.size s

	    fun iter(i,h) =
		if i = n then h else
		let
		    val c  = fromInt(Char.ord(String.sub(s,i)))
		    val h' = (h << 0w4) + c
		    val g  = h' andb 0wxf00000
		in
		    iter(Int.+(i,1), h' xorb (g >> 0w16))
		end
	in
	    toInt(iter(0,0w0))
	end
    
  end
