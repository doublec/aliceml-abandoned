(*
 * Global stamp generator.
 *)


structure GlobalStamp :> GLOBAL_STAMP  =
  struct

    datatype stamp		= GEN of int | STR of string
    type     t			= stamp

    val r			= ref 0

    fun new()			= (r := !r + 1; GEN(!r))

    fun fromString s		= STR s

    fun toString(GEN n)		= Int.toString n
      | toString(STR s)		= s

    fun compare(GEN n1, GEN n2)	= Int.compare(n1,n2)
      | compare(STR s1, STR s2)	= String.compare(s1,s2)
      | compare(GEN n1, STR s2)	= LESS
      | compare(STR s1, GEN n2)	= GREATER

    fun hash(GEN n)		= n
      | hash(STR s)		= StringHashKey.hash s

  end
