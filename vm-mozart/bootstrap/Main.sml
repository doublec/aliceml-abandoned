fun processString process source = process source

fun processFile process name =
    let
	val file   = TextIO.openIn name
	val source = TextIO.inputAll file
	val _      = TextIO.closeIn file
    in
	processString process source
    end

fun elab x     = (ElaborationPhase.elab (Env.copy Env0.E0) o abstract) x
val translate  = TranslationPhase.translate o abstract
val imperatify = MatchCompilationPhase.translate o translate

fun ozify outstream s =
    let
	val component = imperatify s
    in
	OzifyImperativeGrammar.outputComponent (outstream, component);
	TextIO.output1 (outstream, #"\n")
    end

val ozifyFileToStdOut = processFile (ozify TextIO.stdOut)
