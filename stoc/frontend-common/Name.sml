structure Name :> NAME =
  struct

    datatype name = ExId of string | InId
    type     t    = name

    fun compare(ExId s1, ExId s2)	= String.compare(s1,s2)
      | compare(ExId _,  InId)		= GREATER
      | compare(InId,    ExId _)	= LESS
      | compare(InId,    InId)		= EQUAL

    fun hash(ExId s)			= StringHashKey.hash s
      | hash InId			= 0

    fun toString(ExId s)		= s
      | toString InId			= "?"

  end
