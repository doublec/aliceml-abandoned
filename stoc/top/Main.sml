structure Main :> MAIN =
  struct

    fun failure(Crash.CRASH s)        = TextIO.print(s ^ "\n")
      | failure(Error.ERROR((l,r),s)) =
	TextIO.print(Int.toString l ^ "-" ^ Int.toString r ^ ": " ^ s ^ "\n")
      | failure _ = ()

    fun processString process source =
	process (source ^ ";") handle x => ( failure x; raise x )

    fun processFile process name =
	let
	    val file   = TextIO.openIn name
	    val source = TextIO.inputAll file
	    val _      = TextIO.closeIn file
	in
	    processString process source
	end

    val parse      = ParsingPhase.parse
    val abstract   = AbstractionPhase.translate BindEnv0.E0 o parse
    val translate  = TranslationPhase.translate o abstract
    val simplify   = MatchCompilationPhase.simplify o translate

    fun ozify name s =
	let
	    val decs = simplify s
	    val file = TextIO.openOut name
	in
	    OzifySimplified.outputList OzifySimplified.outputDec
				       (file,decs) ;
	    TextIO.output1 (file,#"\n") ;
	    TextIO.closeOut file
	end

    fun ozifyToStream file s =
	let
	    val decs = simplify s
	in
	    OzifySimplified.outputList OzifySimplified.outputDec
				       (file,decs) ;
	    TextIO.output1 (file,#"\n")
	end

    val parseString		= processString parse
    val parseFile		= processFile parse

    val abstractString		= processString abstract
    val abstractFile		= processFile abstract

    val translateString		= processString translate
    val translateFile		= processFile translate

    val simplifyString		= processString simplify
    val simplifyFile		= processFile simplify

    fun ozifyString(s,f)	= processString (ozify f) s
    fun ozifyFile(s,f)		= processFile (ozify f) s

    fun ozifyStringToStream(s,f)= processString (ozifyToStream f) s
    fun ozifyFileToStream(s,f)	= processFile (ozifyToStream f) s

  end
