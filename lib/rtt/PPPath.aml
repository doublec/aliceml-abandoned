structure PPPath :> PP_PATH =
  struct

    (* Import *)

    open PathPrivate
    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    fun ppName n		= text(Name.toString n)
    fun ppLab l			= text(Label.toString l)

    fun ppHiddenLab(0,l)	= ppLab l
      | ppHiddenLab(i,l)	= text "?" ^^ ppHiddenLab(i-1, l)

    fun ppPath (_, ref p')	= ppPath' p'
    and ppPath'(PLAIN n)	= ppName n
      | ppPath'(DOT(p,l,i))	= ppPath p ^^ text "." ^^ ppHiddenLab(i, l)

  end
