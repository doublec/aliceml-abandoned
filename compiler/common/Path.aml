(* Since SML allows multiple definitions of the same id in a structure,
   labels are not enough for paths. So we added an index. *)

structure Path :> PATH =
  struct

  (* Types *)

    type stamp = Stamp.t
    type lab   = Lab.t

    datatype path = PLAIN of stamp * lab * int | DOT of path * lab * int
    type t = path

    type subst = path StampMap.t


  (* Ordering and hashing *)

    fun compare(PLAIN _,       DOT _)	      = LESS
      | compare(DOT _,         PLAIN _)	      = GREATER
      | compare(PLAIN(x1,_,_), PLAIN(x2,_,_)) = Stamp.compare(x1,x2)
      | compare(DOT(p1,l1,n1), DOT(p2,l2,n2)) = case compare(p1,p2)
						  of r as (LESS|GREATER) => r
						   | EQUAL =>
						case Lab.compare(l1,l2)
						  of r as (LESS|GREATER) => r
						   | EQUAL =>
					        Int.compare(n1,n2)

    fun hash(PLAIN(x,_,_)) = Stamp.hash x
      | hash(DOT(p,l,n))   = hash p + Lab.hash l


  (* Substitution *)

    fun substitute(subst, DOT(p,l,n))		= DOT(substitute(subst,p), l, n)
      | substitute(subst, p as PLAIN(z,l,n))	= case StampMap.lookup(subst, z)
						    of SOME p' => DOT(p', l, n)
						     | NONE    => p

  end
