(* Bootstrap Composer *)

signature COMPOSER' =
    sig
	structure Sig: SIGNATURE

	exception Corrupt

	val sign:	Url.t -> Sig.t		(* [Corrupt, IO.Io] *)
	val start:	Url.t -> unit		(* [Corrupt, IO.Io] *)

	val setAcquisitionMethod: (Url.t -> Sig.t) -> unit
    end

structure Composer :> COMPOSER' where type Sig.t = Inf.sign =
    struct
	structure Sig = Signature

	exception Corrupt

	val acquire: (Url.t -> Sig.t) ref =
	    ref (fn _ => raise Crash.Crash "Composer.acquire")

	fun setAcquisitionMethod f = acquire := f

	structure UrlMap = MakeHashImpMap(FromEqHashKey(Url))

	val signTable: Sig.t UrlMap.t = UrlMap.new ()

	fun sign url =
	    case UrlMap.lookup (signTable, url) of
		SOME sign => sign
	      | NONE =>
		    let
			val sign = !acquire url
		    in
			UrlMap.insertDisjoint (signTable, url, sign);
			sign
		    end

	fun start url = ()
    end

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

    val comifyStringToStdOut	= processString (comify TextIO.stdOut)
    val comifyFileToStdOut	= processFile (comify TextIO.stdOut)

    fun comifyStringToFile(s,n)	= processString (toFile comify n) s
    fun comifyFileToFile(n1,n2)	= processFile (toFile comify n2) n1

    (* Tell the composer how to compile Alice source files *)

    fun parseFileUrl url =
	(case (Url.getScheme url, Url.getAuthority url) of
	     ((NONE | SOME "file"), NONE) => ()
	   | _ => raise Crash.Crash "Main.parseFileUrl";
	 Url.setScheme (url, NONE);
	 Url.toString url)

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

    fun compileForMozart (sourceFilename, targetFilename) =
	(TextIO.print ("### compiling file " ^ sourceFilename ^ "\n");
	 processFile (mozartify sourceFilename targetFilename) sourceFilename
	 before TextIO.print ("### wrote file " ^ targetFilename ^ "\n"))

    fun acquireSign url =
	let
	    val targetFilename = parseFileUrl url
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

  end
