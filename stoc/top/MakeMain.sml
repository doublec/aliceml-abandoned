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
			 if Source.url desc =
			     SOME (Url.fromString (homedir ^ "/Base.dll.sig"))
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
		    (case OS.Process.getEnv "STOCKHOME" of
			 SOME s => s ^ "/"
		       | NONE => "") ^
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
	      \Options:\n\
	      \\t--noimplicitimport\n\
	      \\t\tDo not make the SML Standard Basis available.\n\
	      \\t--outputassembly\n\
	      \\t\tWrite an .ozm file with the assembly code.\n")

	fun stoc' ["--replacesign", infile, signfile, outfile] =
	    (Pickle.replaceSign (Url.fromString infile,
				 compileSign signfile, outfile);
	     OS.Process.success)
	  | stoc' ([infile] | ["-c", infile]) =
	    stoc_c (infile, basename infile ^ ".ozf")
	  | stoc' ["-x", infile] =
	    stoc_x (infile, basename infile)
	  | stoc' ([infile, "-o", outfile] | ["-c", infile, "-o", outfile]) =
	    stoc_c (infile, outfile)
	  | stoc' ["-x", infile, "-o", outfile] =
	    stoc_x (infile, outfile)
	  | stoc' _ = (usage (); OS.Process.failure)

	fun options ("--noimplicitimport"::rest) =
	    (Switches.implicitImport := false; options rest)
	  | options ("--outputassembly"::rest) =
	    (Switches.outputAssembly := true; options rest)
	  | options ("--noprintcomponentsig"::rest) =
	    (Switches.printComponentSig := false; options rest)
	  | options rest = rest

	fun defaults () =
	    (Switches.implicitImport := true;
	     Switches.outputAssembly := false;
	     Switches.printComponentSig := true)

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
	    structure AbstractionPhase = MakeAbstractionPhase(Composer)
	    structure ElaborationPhase =
		MakeElaborationPhase(structure Composer = Composer
				     structure Switches = Switches)

	    fun parse' x     = ParsingPhase.translate () x
	    fun abstract' x  = AbstractionPhase.translate (BindEnv.new()) x
	    fun elab' x      = ElaborationPhase.translate (Env.new()) x
	    fun translate' x = TranslationPhase.translate () x
	    fun flatten' x   = BackendCommon.translate () x

	    fun (f o g) (desc, x) = f (desc, g (desc, x))

	    fun source (_, s) = Source.fromString s
	    val parse         = parse' o source
	    val abstract      = abstract' o parse
	    val elab          = elab' o abstract
	    val translate     = translate' o elab
	    val flatten       = flatten' o translate
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
