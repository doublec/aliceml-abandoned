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

    fun parse' x     = ParsingPhase.translate () x
    fun abstract' x  = AbstractionPhase.translate (BindEnv.clone BindEnv0.E0) x
    fun elab' x      = ElaborationPhase.translate (Env.clone Env0.E0) x
    fun translate' x = TranslationPhase.translate () x
    fun flatten' x   = FlatteningPhase.translate () x
    fun ilify' x     = CodeGenPhase.genComponent x

    val parse        = parse' o Source.fromString
    val abstract     = abstract' o parse
    val elab         = elab' o abstract
    val translate    = translate' o elab
    val flatten      = flatten' o translate
    val ilify        = ilify' o flatten

    fun debug outstream s =
	let
	    val x = flatten s
	    val _ = LivenessAnalysisPhase.annotate x
	    val s' = OutputFlatGrammar.outputComponent x
	in
	    TextIO.output (outstream, s')
	end

    fun ozify outstream inFilename s =
	let
	    val component = flatten s
	in
	    OzifyFlatGrammar.externalize (outstream, (inFilename, component));
	    TextIO.output1 (outstream, #"\n")
	end

    fun mozartify inFilename outFilename s =
	let
	    val component as (_, (_, exportSign)) = flatten s
	    val engine = MozartEngine.start ()
	in
	    MozartTarget.save engine outFilename
	    (MozartGenerationPhase.translate inFilename component);
	    exportSign
	end

    fun comify outstream s =
	let
	    val component = ilify s
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

    val flattenString		= processString flatten
    val flattenFile		= processFile flatten

    val debugStringToStdOut	= processString (debug TextIO.stdOut)
    val debugFileToStdOut	= processFile (debug TextIO.stdOut)

    fun debugStringToFile(s,n)	= processString (toFile debug n) s
    fun debugFileToFile(n1,n2)	= processFile (toFile debug n2) n1

    val ozifyStringToStdOut	= processString (ozify TextIO.stdOut "")
    fun ozifyFileToStdOut(n)	= processFile (ozify TextIO.stdOut n) n

    fun ozifyStringToFile(s,n)	= processString (toFile ozify n) s
    fun ozifyFileToFile(n1,n2)	= processFile (toFile ozify n2) n1

    fun compileForMozart(n1,n2)	= processFile (mozartify n1 n2) n1

    val comifyStringToStdOut	= processString (comify TextIO.stdOut)
    val comifyFileToStdOut	= processFile (comify TextIO.stdOut)

    fun comifyStringToFile(s,n)	= processString (toFile comify n) s
    fun comifyFileToFile(n1,n2)	= processFile (toFile comify n2) n1

  end
