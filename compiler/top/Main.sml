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

structure Main :> MAIN =
  struct

    structure Switches = Switches

    fun crashGuard process x =
	process x
	handle exn as Crash.Crash message =>
	    ( TextIO.output(TextIO.stdErr, "CRASH: " ^ message ^ "\n")
	    ; raise exn
	    )

    fun processString process source =
	crashGuard process (Source.stringDesc, source)

    fun processFile process name =
	let
	    val file   = TextIO.openIn name
	    val source = TextIO.inputAll file
	    val _      = TextIO.closeIn file
	in
	    crashGuard process (Source.urlDesc (Url.fromString name), source)
	end

    (*DEBUG*)
    local
	structure AbstractionPhase = MakeAbstractionPhase(Composer)
	structure ElaborationPhase = MakeElaborationPhase(Composer)

	fun parse' x     = ParsingPhase.translate () x
	fun abstract' x  = AbstractionPhase.translate
			   (BindEnv.clone BindEnv0.E0) x
	fun elab' x      = ElaborationPhase.translate (Env.clone Env0.E0) x
	fun translate' x = TranslationPhase.translate () x
	fun flatten' x   = BackendCommon.translate () x
	fun mozartify' x = MozartGenerationPhase.translate () x

	fun (f o g) (desc, x) = f (desc, g (desc, x))

	fun source (_, s) = Source.fromString s
	val parse         = parse' o source
	val abstract      = abstract' o parse
	val elab          = elab' o abstract
	val translate     = translate' o elab
	val flatten       = flatten' o translate
	val mozartify     = mozartify' o flatten
    in
	val parseString		= processString parse
	val parseFile		= processFile parse

	val abstractString	= processString abstract
	val abstractFile	= processFile abstract

	val elabString		= processString elab
	val elabFile		= processFile elab

	val translateString	= processString translate
	val translateFile	= processFile translate

	val flattenString	= processString flatten
	val flattenFile		= processFile flatten
    end

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

    local
	val engine: MozartEngine.t option ref = ref NONE

	fun compile outFilename (desc, s) =
	    let
		val _ =
		    if (isSome (!engine)) then ()
		    else engine := SOME (MozartEngine.start ())
		val (_, component as (_, (_, (_, exportSign)))) =
		    Compiler.compile (Compiler.initial, desc,
				      Source.fromString s)
	    in
		MozartTarget.save (valOf (!engine)) outFilename component;
		exportSign
	    end

	val fileStack: string list ref = ref nil
    in
	fun compileForMozart (sourceFilename, targetFilename) =
	    (TextIO.print ("### compiling file " ^ sourceFilename ^ "\n");
	     fileStack := sourceFilename::(!fileStack);
	     processFile (compile targetFilename) sourceFilename
	     before (TextIO.print ("### wrote file " ^ targetFilename ^ "\n");
		     case !fileStack of
			 _::(rest as resumeFilename::_) =>
			     (fileStack := rest;
			      TextIO.print ("### resuming compilation of " ^
					    resumeFilename ^ "\n"))
		       | _ => ()))
    end

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

  end
