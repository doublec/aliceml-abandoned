(*
 * Authors:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt and Andreas Rossberg, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

functor MakeMain(structure Composer: COMPOSER'
		     where type Sig.t = Signature.t
		 structure Compiler: COMPILER
		     where Target.Sig = Signature
		 val executableHeader: string): MAIN =
    struct
	structure Composer = Composer
	structure Switches = Compiler.Switches

	fun readFile filename =
	    let
		val file   = TextIO.openIn filename
		val source = TextIO.inputAll file
		val _      = TextIO.closeIn file
	    in
		source
	    end

	fun processBasic process (desc, s) =
	    process
	    (desc,
	     if !Switches.implicitImport then
		 case OS.Process.getEnv "STOCKHOME" of
		     SOME homedir =>
			 if Url.toString (valOf (Source.url desc)) =
			     (homedir ^ "/lib/Base.ozf.sig")
			 then s
			 else
			     String.map (fn #"\n" => #" " | c => c)
			     (readFile (homedir ^ "/Default.import")) ^
			     "\n" ^ s
		   | NONE =>
			 (TextIO.print
			  "### warning: Default.import not found\n"; s)
	     else s)
	    handle exn as Crash.Crash message =>
		(TextIO.output(TextIO.stdErr, "CRASH: " ^ message ^ "\n");
		 raise exn)

	fun processString process source =
	    processBasic process (Source.stringDesc, source)

	fun processFile process filename =
	    processBasic process (Source.urlDesc (Url.fromString filename),
				  readFile filename)

	local
	    fun compileSign' (desc, s) =
		let
		    val _ = TextIO.print ("### reading signature" ^
					  (case Source.url desc of
					       SOME url =>
						   " from " ^
						   Url.toString url ^ "\n"
					     | NONE => "\n"))
		    val (_, target) =
			Compiler.compile (Compiler.empty, desc,
					  Source.fromString s)
		    val _ = TextIO.print "### done\n"
		in
		    case Inf.items (Compiler.Target.sign target) of
			[item] =>
			let
			    val inf = (valOf (#3 (Inf.asInfItem item)))
			in
			    Inf.strengthen(Path.invent(), inf);
			    Inf.asSig inf
			end
		      | _ => raise Crash.Crash "MakeMain.compileSign"
		end
	in
	    fun compileSign filename = processFile compileSign' filename
	end

	local
	    fun compile' (outFilename, header) (desc, s) =
		let
		    val (_, target) =
			Compiler.compile (Compiler.empty, desc,
					  Source.fromString s)
		in
		    (*--** header *)
		    Compiler.Target.save (Compiler.Target.C.new ())
		    outFilename target;
		    Compiler.Target.sign target
		end

	    val fileStack: string list ref = ref nil
	in
	    fun compile (sourceFilename, targetFilename, header) =
		(TextIO.print ("### compiling file " ^ sourceFilename ^ "\n");
		 fileStack := sourceFilename::(!fileStack);
		 processFile (compile' (targetFilename, header)) sourceFilename
		 before (TextIO.print ("### wrote file " ^
				       targetFilename ^ "\n");
			 case fileStack of
			     ref (_::(rest as resumeFilename::_)) =>
				 (fileStack := rest;
				  TextIO.print
				  ("### resuming compilation of " ^
				   resumeFilename ^ "\n"))
			   | ref _ => ()))
	end

	(* Tell the composer how to compile Alice source files *)

	fun parseUrl url =
	    case (Url.getScheme url, Url.getAuthority url) of
		(NONE, NONE) =>
		    Url.toString (Url.setScheme (url, NONE))
	      | (SOME "file", NONE) =>
		    Url.toString (Url.setScheme (url, NONE))
	      | (SOME "x-alice", NONE) =>
(*--**UNFINISHED: This line is necessary for the COM+ backend *)
		    (case OS.Process.getEnv "STOCKHOME" of
			 SOME s => s ^ "/"
		       | NONE => "") ^
(**)
		    Url.toString (Url.setScheme (Url.makeRelativePath url,
						 NONE))
	      | _ => raise Crash.Crash "MakeMain.parseUrl"

	fun existsFile filename =
	    (TextIO.closeIn (TextIO.openIn filename); true)
	    handle IO.Io _ => false

	fun changeExtension (filename, fro, to) =
	    let
		val n = String.size filename
		val m = String.size fro
	    in
		if n > m andalso String.substring (filename, n - m, m) = fro
		then
		    let
			val newname =
			    String.substring (filename, 0, n - m) ^ to
		    in
			if existsFile newname then SOME newname
			else NONE
		    end
		else NONE
	    end

	exception RETURN of Composer.Sig.t

	fun acquireSign url =
	    (case Pickle.loadSign url of
		 SOME sign =>
		     (TextIO.print ("### loaded signature from " ^
				    Url.toString url ^ "\n");
		      raise RETURN sign)
	       | NONE => ();
	     let
		 val targetFilename = parseUrl url
		 val sigFilename = targetFilename ^ ".sig"
	     in
		 if existsFile sigFilename then
		     raise RETURN (compileSign sigFilename)
		 else ();
		 case changeExtension (targetFilename, ".dll", ".aml") of
		     SOME sourceFilename =>
			 raise RETURN (compile
				       (sourceFilename, targetFilename, ""))
		   | NONE => ();
		 case changeExtension (targetFilename, ".ozf", ".aml") of
		     SOME sourceFilename =>
			 raise RETURN (compile
				       (sourceFilename, targetFilename, ""))
		   | NONE => ();
		 case changeExtension (targetFilename, ".ozf", ".sml") of
		     SOME sourceFilename =>
			 raise RETURN (compile
				       (sourceFilename, targetFilename, ""))
		   | NONE => ();
		 case changeExtension (targetFilename, ".ozf", ".sig") of
		     SOME sourceFilename =>
			 raise RETURN (compile
				       (sourceFilename, targetFilename, ""))
		   | NONE => ();
		 TextIO.print ("### warning: could not locate source for " ^
			       targetFilename ^ "\n");
		 Inf.empty ()
	     end) handle RETURN sign => sign

	val _ = Composer.setAcquisitionMethod acquireSign

	(* Command Line Processing *)

	fun basename filename =
	    let
		fun cutPath ((#"/" | #"\\")::rest) = nil
		  | cutPath (c::rest) = c::cutPath rest
		  | cutPath nil = nil
		val cs = cutPath (List.rev (String.explode filename))
		fun cutExtension (#"."::rest) =
		    (case rest of
			 (#"/" | #"\\")::_ => cs
		       | _::_ => rest
		       | nil => cs)
		  | cutExtension ((#"/" | #"\\")::_) = cs
		  | cutExtension (_::rest) = cutExtension rest
		  | cutExtension nil = cs
	    in
		String.implode (List.rev (case cs of
					      #"."::_ => cs
					    | _ => cutExtension cs))
	    end

	fun stoc_c (infile, outfile) =
	    (compile (infile, outfile, "");
	     OS.Process.success)

	fun stoc_x (infile, outfile) =
	    (compile (infile, outfile, executableHeader);
	     OS.Process.success
(*--**UNFINISHED
	     case SMLofNJ.SysInfo.getOSKind () of
		 SMLofNJ.SysInfo.WIN32 => OS.Process.success
	       | _ => OS.Process.system ("chmod +x " ^ outfile)
*)
)

	fun usage () =
	    TextIO.output
	    (TextIO.stdErr,
	     "Usage:\n\
	      \\tstoc [<option> ...] [-c|-x] <input file> \
	      \[-o <output file>]\n\
	      \\tstoc --replacesign <input url> <signature file> \
	      \<output file>\n\
	      \Bootstrap options:\n\
	      \\t--(no-)implicit-import\n\
	      \\t\tWhether the SML Standard Basis is made available.\n\
	      \\t--rtt-level=no\n\
	      \\t\tDo not generate code for runtime types.\n\
	      \\t--rtt-level=core\n\
	      \\t\tDo only generate code for core runtime types.\n\
	      \Debug options:\n\
	      \\t--(no-)dryrun\n\
	      \\t\tCompile standard input, not writing any output.\n\
	      \\t--(no-)dump-phases\n\
	      \\t\tTrace the running phases.\n\
	      \\t--(no-)dump-abstraction-result\n\
	      \\t\tDump abstract representation.\n\
	      \\t--(no-)dump-elaboration-result\n\
	      \\t\tDump abstract representation after elaboration.\n\
	      \\t--(no-)dump-elaboration-sig\n\
	      \\t\tDump component signatures after elaboration.\n\
	      \\t--(no-)dump-intermediate\n\
	      \\t\tDump intermediate representation.\n\
	      \\t--(no-)check-intermediate\n\
	      \\t\tType-check intermediate representation.\n\
	      \\t--(no-)dump-flattening-result\n\
	      \\t\tDump flat representation after flattening.\n\
	      \\t--(no-)dump-value-propagation-result\n\
	      \\t\tDump flat representation after value propagation.\n\
	      \\t--(no-)dump-liveness-analysis-intermediate\n\
	      \\t\tDump flat representation with liveness annotations.\n\
	      \\t--(no-)dump-liveness-analysis-result\n\
	      \\t\tDump flat representation after liveness analysis.\n\
	      \\t--(no-)dump-dead-code-elimination-result\n\
	      \\t\tDump flat representation after dead code elimination.\n\
	      \\t--(no-)dump-target\n\
	      \\t\tDump target code representation.\n")

	fun stoc' ["--replacesign", infile, signfile, outfile] =
	    (Pickle.replaceSign (Url.fromString infile,
				 compileSign signfile, outfile);
	     OS.Process.success)
	  | stoc' ["--dryrun"] =
	    let
		val s = TextIO.inputAll TextIO.stdIn
	    in
		Compiler.compile (Compiler.empty, Source.stringDesc,
				  Source.fromString s);
		OS.Process.success
	    end
	  | stoc' ([infile] | ["-c", infile]) =
	    stoc_c (infile, basename infile ^ ".ozf")
	  | stoc' ["-x", infile] =
	    stoc_x (infile, basename infile)
	  | stoc' ([infile, "-o", outfile] | ["-c", infile, "-o", outfile]) =
	    stoc_c (infile, outfile)
	  | stoc' ["-x", infile, "-o", outfile] =
	    stoc_x (infile, outfile)
	  | stoc' _ = (usage (); OS.Process.failure)

	val booleanSwitches =
	    [("implicit-import", Switches.Bootstrap.implicitImport),
	     ("dump-phases", Switches.Debug.dumpPhases),
	     ("dump-abstraction-result", Switches.Debug.dumpAbstractionResult),
	     ("dump-elaboration-result", Switches.Debug.dumpElaborationResult),
	     ("dump-elaboration-sig", Switches.Debug.dumpElaborationSig),
	     ("dump-intermediate", Switches.Debug.dumpIntermediate),
	     ("check-intermediate", Switches.Debug.checkIntermediate),
	     ("dump-flattening-result", Switches.Debug.dumpFlatteningResult),
	     ("dump-value-propagation-result",
	      Switches.Debug.dumpValuePropagationResult),
	     ("dump-liveness-analysis-intermediate",
	      Switches.Debug.dumpLivenessAnalysisIntermediate),
	     ("dump-liveness-analysis-result",
	      Switches.Debug.dumpLivenessAnalysisResult),
	     ("dump-dead-code-elimination-result",
	      Switches.Debug.dumpDeadCodeEliminationResult),
	     ("dump-target",
	      Switches.Debug.dumpTarget)]

	fun checkBooleanSwitches (s, (name, switch)::rest) =
	    if "--" ^ name = s then (switch := true; true)
	    else if "--no-" ^ name = s then (switch := false; true)
	    else checkBooleanSwitches (s, rest)
	  | checkBooleanSwitches (_, nil) = false

	fun options ("--rtt-level=no"::rest) =
	    (Switches.Bootstrap.rttLevel := Switches.Bootstrap.NO_RTT;
	     options rest)
	  | options ("--rtt-level=core"::rest) =
	    (Switches.Bootstrap.rttLevel := Switches.Bootstrap.CORE_RTT;
	     options rest)
	  | options (s::rest) =
	    if checkBooleanSwitches (s, booleanSwitches) then options rest
	    else s::rest
	  | options nil = nil

	fun defaults () = () (* override defaults from MakeSwitches here *)

	fun stoc arguments =
	    (defaults (); stoc' (options arguments))
	    handle Error.Error (_, _) => OS.Process.failure
		 | e =>   (*--**DEBUG*)
		       (TextIO.output
			(TextIO.stdErr,
			 "uncaught exception " ^ exnName e ^ "\n");
			OS.Process.failure)

	(*DEBUG*)
	local
	    structure ParsingPhase =
		  MakeTracingPhase(
			structure Phase    = MakeParsingPhase(Switches)
			structure Switches = Switches
			val name = "Parsing"
		  )
	    structure AbstractionPhase =
		  MakeTracingPhase(
			structure Phase    = MakeAbstractionPhase(Composer)
			structure Switches = Switches
			val name = "Abstraction"
		  )
	    structure AbstractionPhase =
		  MakeDumpingPhase(
			structure Phase    = AbstractionPhase
			structure Switches = Switches
			val header = "Abstract Syntax"
			val pp     = PPAbstractGrammar.ppComp
			val switch = Switches.Debug.dumpAbstractionResult
		  )
	    structure ElaborationPhase =
		  MakeTracingPhase(
			structure Phase    = MakeElaborationPhase(Composer)
			structure Switches = Switches
			val name = "Elaboration"
		  )
	    structure ElaborationPhase =
		  MakeDumpingPhase(
			structure Phase    = ElaborationPhase
			structure Switches = Switches
			val header = "Component Signature"
			val pp     = PPInf.ppSig o #sign o TypedGrammar.infoComp
			val switch = Switches.Debug.dumpElaborationSig
		  )
	    structure TranslationPhase =
		  MakeTracingPhase(
			structure Phase    = MakeTranslationPhase(Switches)
			structure Switches = Switches
			val name = "Translation"
		  )
	    structure TranslationPhase =
		  MakeDumpingPhase(
			structure Phase    = TranslationPhase
			structure Switches = Switches
			val header = "Intermediate Syntax"
			val pp     = PPIntermediateGrammar.ppComp
			val switch = Switches.Debug.dumpIntermediate
		  )
	    structure BackendCommon = MakeBackendCommon(Switches)

	    fun parse' x     = ParsingPhase.translate () x
	    fun abstract' x  = AbstractionPhase.translate (BindEnv.new()) x
	    fun elab' x      = let val comp = ElaborationPhase.translate
						(Env.new()) x
				   val i = TypedGrammar.infoComp comp
			       in  BindEnvFromSig.envFromSig(#region i,#sign i);
				   comp
			       end
	    fun translate' x = TranslationPhase.translate () x
	    fun flatten' x   = BackendCommon.translate () x

	    infix 3 oo
	    fun (f oo g) (desc, x) = f (desc, g (desc, x))

	    fun source (_, s) = Source.fromString s
	    val parse         = parse' oo source
	    val abstract      = abstract' oo parse
	    val elab          = elab' oo abstract
	    val translate     = translate' oo elab
	    val flatten       = flatten' oo translate
	in
	    val parseString	= processString parse
	    val parseFile	= processFile parse

	    val abstractString	= processString abstract
	    val abstractFile	= processFile abstract

	    val elabString	= processString elab
	    val elabFile	= processFile elab

	    val translateString	= processString translate
	    val translateFile	= processFile translate

	    val flattenString	= processString flatten
	    val flattenFile	= processFile flatten
	end
    end
