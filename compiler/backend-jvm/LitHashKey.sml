structure LitHashKey =
  struct
      open Main
      datatype lit = datatype IntermediateGrammar.lit

		     type t = lit

    open Word

    fun hash (WordLit lw) = LargeWord.toInt(LargeWord.andb (lw,0wxf00000))
      | hash (IntLit i) = LargeWord.toInt(LargeWord.andb
					  (LargeWord.fromLargeInt i,0wxf00000))
      | hash (CharLit c) = ord c
      | hash (StringLit s) = StringHashKey.hash s
      | hash (RealLit s) = StringHashKey.hash s
  end
