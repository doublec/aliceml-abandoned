(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *   Andreas Rossberg, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure AbstractionPhase = MakeAbstractionPhase(Composer)
structure ElaborationPhase = MakeElaborationPhase(Composer)

(* Test *)
structure FrontendSML    = MakeFrontendSML(Composer)
structure FrontendCommon = MakeFrontendCommon(Composer)


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

    local
	val engine: MozartEngine.t option ref = ref NONE
    in
	fun mozartify inFilename outFilename s =
	    let
		val component as (_, (_, exportSign)) = flatten s
	    in
		if isSome (!engine) then ()
		else engine := SOME (MozartEngine.start ());
		MozartTarget.save (valOf (!engine)) outFilename
		(MozartGenerationPhase.translate inFilename component);
		exportSign
	    end
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

    val comifyStringToStdOut	= processString (comify TextIO.stdOut)
    val comifyFileToStdOut	= processFile (comify TextIO.stdOut)

    fun comifyStringToFile(s,n)	= processString (toFile comify n) s
    fun comifyFileToFile(n1,n2)	= processFile (toFile comify n2) n1

    (* Tell the composer how to compile Alice source files *)

    fun parseUrl url =
	case (Url.getScheme url, Url.getAuthority url) of
	    ((NONE | SOME "file"), NONE) =>
		Url.toString (Url.setScheme (url, NONE))
	  | (SOME "x-alice", NONE) =>
		Url.toString (Url.setScheme (Url.makeRelativePath url, NONE))
	  | _ => raise Crash.Crash "Main.parseUrl"

    fun existsFile filename =
	(TextIO.closeIn (TextIO.openIn filename); true) handle IO.Io _ => false

    fun changeExtension (filename, fro, to) =
	let
	    val n = String.size filename
	    val m = String.size fro
	in
	    if n > m andalso String.substring (filename, n - m, m) = fro then
		let
		    val newname = String.substring (filename, 0, n - m) ^ to
		in
		    if existsFile newname then SOME newname
		    else NONE
		end
	    else NONE
	end

    fun compileSign filename =
	let
	    val _ =
		TextIO.print ("### reading signature file " ^ filename ^ "\n")
	    val (_, (_, sign)) = translateFile filename
	    val _ = TextIO.print "### done\n"
	in
	    case Inf.items sign of
		[item] => Inf.asSig (valOf (#3 (Inf.asInfItem item)))
	      | _ => raise Crash.Crash "Composer.compileSign"
	end

    val fileStack: string list ref = ref nil

    fun compileForMozart (sourceFilename, targetFilename) =
	(TextIO.print ("### compiling file " ^ sourceFilename ^ "\n");
	 fileStack := sourceFilename::(!fileStack);
	 processFile (mozartify sourceFilename targetFilename) sourceFilename
	 before (TextIO.print ("### wrote file " ^ targetFilename ^ "\n");
		 case !fileStack of
		     _::(rest as resumeFilename::_) =>
			 (fileStack := rest;
			  TextIO.print ("### resuming compilation of " ^
					resumeFilename ^ "\n"))
		   | _ => ()))

    fun acquireSign url =
	let
	    val targetFilename = parseUrl url
	    val sigFilename = targetFilename ^ ".sig"
	in
	    if existsFile sigFilename then compileSign sigFilename
	    else
		case changeExtension (targetFilename, ".ozf", ".sml") of
		    SOME sourceFilename =>
			compileForMozart (sourceFilename, targetFilename)
		  | NONE =>
			case changeExtension (targetFilename,
					      ".ozf", ".sig") of
			    SOME sourceFilename =>
				compileForMozart (sourceFilename,
						  targetFilename)
			  | NONE =>
				(TextIO.print
				 ("### warning: could not locate source for " ^
				  targetFilename ^ "\n");
				 Inf.empty ())
	end

    val _ = Composer.setAcquisitionMethod acquireSign
    val _ = Switches.printComponentSig := false

  end
