structure Path :> PATH =
  struct

    type stamp = Stamp.t
    type name  = Name.t
    type lab   = Lab.t

    datatype path = PLAIN of stamp * name | DOT of path * lab

    type t = path

    (* Ordering and hashing *)

    fun compare(PLAIN _,     DOT _)	  = LESS
      | compare(DOT _,       PLAIN _)	  = GREATER
      | compare(PLAIN(x1,_), PLAIN(x2,_)) = Stamp.compare(x1,x2)
      | compare(DOT(p1,l1),  DOT(p2,l2))  = case compare(p1,p2)
					      of EQUAL => Lab.compare(l1,l2)
					       | other => other
    fun hash(PLAIN(x,_)) = Stamp.hash x
      | hash(DOT(p,l))   = hash p + Lab.hash l

  end
