structure Label :> LABEL =
  struct

    datatype lab = NUM of LargeInt.int | ALPHA of string	(* [lab,l] *)
    type     t   = lab


    (* Conversions *)

    fun fromInt n		= NUM(LargeInt.fromInt n)
    fun fromLargeInt n		= NUM n
    fun fromString s		= case LargeInt.fromString s
				    of SOME n => NUM n
				     | NONE   => ALPHA s

    fun fromName(Name.ExId s)	= fromString s
      | fromName(Name.InId)	= raise Domain

    fun toName(ALPHA s)		= Name.ExId s
      | toName(NUM n)		= Name.ExId(LargeInt.toString n)

    fun toString(NUM n)		= LargeInt.toString n
      | toString(ALPHA s)	= s

    fun toLargeInt(NUM n)	= SOME n
      | toLargeInt(ALPHA s)	= NONE


    (* Ordering and hashing *)

    fun compare(NUM n1,   NUM n2)	= LargeInt.compare(n1,n2)
      | compare(ALPHA s1, ALPHA s2)	= String.compare(s1,s2)
      | compare(NUM n1,   ALPHA s2)	= LESS
      | compare(ALPHA s1, NUM n2)	= GREATER

    fun hash(NUM n)			= LargeIntHashKey.hash n
      | hash(ALPHA s)			= StringHashKey.hash s

  end
