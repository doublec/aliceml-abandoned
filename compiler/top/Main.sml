structure Main :> MAIN =
  struct

    fun processString process source =
	process source
	handle exn as Crash.Crash message =>
	    ( TextIO.output(TextIO.stdErr, "CRASH: " ^ message ^ "\n")
	    ; raise exn
	    )

    fun processFile process name =
	let
	    val file   = TextIO.openIn name
	    val source = TextIO.inputAll file
	    val _      = TextIO.closeIn file
	in
	    processString process source
	end

    fun toFile process name s =
	let
	    val file = TextIO.openOut name
	in
	    process file s handle x => ( TextIO.closeOut file ; raise x ) ;
	    TextIO.closeOut file
	end

    fun abstract' x = AbstractionPhase.translate (BindEnv.copy BindEnv0.E0) x
    fun elab' x     = ElaborationPhase.elab (Env.copy Env0.E0) x

    val parse      = ParsingPhase.parse o Source.fromString
    val abstract   = abstract' o parse
    val elab       = elab' o abstract
    val translate  = TranslationPhase.translate o elab
    val imperatify = MatchCompilationPhase.translate o translate
    val illify     = CodeGenPhase.genComponent o imperatify

    fun ozify outstream s =
	let
	    val component = imperatify s
	in
	    OzifyImperativeGrammar.outputComponent (outstream, component);
	    TextIO.output1 (outstream, #"\n")
	end

    fun debug outstream s =
	let
	    val x = imperatify s
	    val _ = LivenessAnalysisPhase.annotate x
	    val s' = OutputImperativeGrammar.outputComponent x
	in
	    TextIO.output (outstream, s')
	end

    fun comify outstream s =
	let
	    val component = illify s
	in
	    IL.outputProgram (outstream, component);
	    TextIO.output1 (outstream, #"\n")
	end

    val parseString		= processString parse
    val parseFile		= processFile parse

    val abstractString		= processString abstract
    val abstractFile		= processFile abstract

    val elabString		= processString elab
    val elabFile		= processFile elab

    val translateString		= processString translate
    val translateFile		= processFile translate

    val imperatifyString	= processString imperatify
    val imperatifyFile		= processFile imperatify

    val ozifyStringToStdOut	= processString (ozify TextIO.stdOut)
    val ozifyFileToStdOut	= processFile (ozify TextIO.stdOut)

    fun ozifyStringToFile(s,n)	= processString (toFile ozify n) s
    fun ozifyFileToFile(n1,n2)	= processFile (toFile ozify n2) n1

    val debugStringToStdOut	= processString (debug TextIO.stdOut)
    val debugFileToStdOut	= processFile (debug TextIO.stdOut)

    fun debugStringToFile(s,n)	= processString (toFile debug n) s
    fun debugFileToFile(n1,n2)	= processFile (toFile debug n2) n1

    val comifyStringToStdOut	= processString (comify TextIO.stdOut)
    val comifyFileToStdOut	= processFile (comify TextIO.stdOut)

    fun comifyStringToFile(s,n)	= processString (toFile comify n) s
    fun comifyFileToFile(n1,n2)	= processFile (toFile comify n2) n1

  end
