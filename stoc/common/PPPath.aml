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

    fun ppPath(ref(PLAIN n))	= ppName n
      | ppPath(ref(URL _))	= raise Crash.Crash "PPPath.ppPath: URL path"
      | ppPath(ref(DOT(p,l,i)))	= (case !p of URL _ => text ""
					    | _     => ppPath p ^^ text ".")
				  ^^ ppHiddenLab(i, l)

  end
