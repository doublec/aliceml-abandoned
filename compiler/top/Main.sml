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

    val parse      = Parse.parse
    val translate  = Translate_Program.translate BindEnv0_Module.E0 o parse

    fun ozify name s =
	let
	    val file = TextIO.openOut name
	    val decs = translate s
	in
	    OzifyIntermediate.output_list OzifyIntermediate.output_dec
					  (file,decs) ;
	    TextIO.closeOut file
	end

    val parseString	= processString parse
    val parseFile	= processFile parse

    val translateString	= processString translate
    val translateFile	= processFile translate

    fun ozifyString(s,f)= processString (ozify f) s
    fun ozifyFile(s,f)	= processFile (ozify f) s

  end
