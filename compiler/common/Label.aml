structure Label :> LABEL =
  struct

    datatype lab = NUM of int | ALPHA of string		(* [lab,l] *)

    type t   = lab


    (* Conversions *)

    fun fromInt n		= NUM n
    fun fromString s		= case Int.fromString s
				    of SOME n => NUM n
				     | NONE   => ALPHA s

    fun fromName(Name.ExId s)	= ALPHA s
      | fromName(Name.InId)	= ALPHA ""

    fun toName(ALPHA "")	= Name.InId
      | toName(ALPHA s)		= Name.ExId s
      | toName(NUM n)		= Name.ExId(Int.toString n)

    fun toString(NUM n)		= Int.toString n
      | toString(ALPHA s)	= s


    (* Ordering and hashing *)

    fun compare(NUM n1,   NUM n2)	= Int.compare(n1,n2)
      | compare(ALPHA s1, ALPHA s2)	= String.compare(s1,s2)
      | compare(NUM n1,   ALPHA s2)	= LESS
      | compare(ALPHA s1, NUM n2)	= GREATER

    fun hash(NUM n)			= n
      | hash(ALPHA s)			= StringHashKey.hash s

  end
