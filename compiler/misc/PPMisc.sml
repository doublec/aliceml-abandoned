(*
 * Miscellaneous pretty printing helpers
 *)

structure PPMisc :> PP_MISC =
  struct

    (* Import *)

    open PrettyPrint

    infixr ^^


    (* Some PP combinators *)

    val nest = nest 3

    fun paren doc = text "(" ^^ fbox(below doc) ^^ text ")"
    fun brace doc = text "{" ^^ fbox(below doc) ^^ text "}"
    fun brack doc = text "[" ^^ fbox(below doc) ^^ text "]"

    fun ppCommaList ppX   []    = empty
      | ppCommaList ppX   [x]   = ppX x
      | ppCommaList ppX (x::xs) = ppX x ^^ text "," ^^ break ^^
				  ppCommaList ppX xs

    fun ppStarList ppX   []     = empty
      | ppStarList ppX   [x]    = ppX x
      | ppStarList ppX (x::xs)  = hbox(ppX x ^^ break ^^ text "*") ^^ break ^^
				  ppStarList ppX xs

    fun ppSeqPrec ppXPrec n []  = empty
      | ppSeqPrec ppXPrec n [x] = ppXPrec n x
      | ppSeqPrec ppXPrec n  xs = paren(ppCommaList (ppXPrec 0) xs)

    fun ppSeq ppX = ppSeqPrec (fn _ => ppX) 0

  end
