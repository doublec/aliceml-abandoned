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

val imperatifyFile = processFile imperatify
