(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

functor MakeBatchCompiler(structure RecursiveCompiler: RECURSIVE_COMPILER
			      where type Composer.Sig.t = Signature.t
			  val executableHeader: string): BATCH_COMPILER =
    struct
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
	    (RecursiveCompiler.compileFileToFile (infile, outfile);
	     OS.Process.success)

	fun stoc_x (infile, outfile) =
	    (*--** header *)
	    (RecursiveCompiler.compileFileToFile (infile, outfile);
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
	      \Warning options:\n\
	      \\t--(no-)warn-shadowing\n\
	      \\t\tWhether to warn about shadowing of identifiers.\n\
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
	      \\t--(no-)dump-value-propagation-context\n\
	      \\t\tDump environment after value propagation.\n\
	      \\t--(no-)dump-value-propagation-result\n\
	      \\t\tDump flat representation after value propagation.\n\
	      \\t--(no-)dump-liveness-analysis-intermediate\n\
	      \\t\tDump flat representation with liveness annotations.\n\
	      \\t--(no-)dump-liveness-analysis-context\n\
	      \\t\t.Dump set of defined stamps after liveness analysis.\n\
	      \\t--(no-)dump-liveness-analysis-result\n\
	      \\t\tDump flat representation after liveness analysis.\n\
	      \\t--(no-)dump-dead-code-elimination-result\n\
	      \\t\tDump flat representation after dead code elimination.\n\
	      \\t--(no-)dump-target\n\
	      \\t\tDump target code representation.\n")

	fun stoc' ["--replacesign", infile, signfile, outfile] =
	    (Pickle.replaceSign (Url.fromString infile,
				 RecursiveCompiler.compileSign signfile,
				 outfile);
	     OS.Process.success)
	  | stoc' ["--dryrun"] =
	    let
		val s = TextIO.inputAll TextIO.stdIn
	    in
		RecursiveCompiler.compileString RecursiveCompiler.empty s;
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

	open RecursiveCompiler.Switches

	val booleanSwitches =
	    [("warn-shadowing", Warn.shadowing),
	     ("implicit-import", Bootstrap.implicitImport),
	     ("dump-phases", Debug.dumpPhases),
	     ("dump-abstraction-result", Debug.dumpAbstractionResult),
	     ("dump-elaboration-result", Debug.dumpElaborationResult),
	     ("dump-elaboration-sig", Debug.dumpElaborationSig),
	     ("dump-intermediate", Debug.dumpIntermediate),
	     ("check-intermediate", Debug.checkIntermediate),
	     ("dump-flattening-result", Debug.dumpFlatteningResult),
	     ("dump-value-propagation-context",
	      Debug.dumpValuePropagationContext),
	     ("dump-value-propagation-result",
	      Debug.dumpValuePropagationResult),
	     ("dump-liveness-analysis-intermediate",
	      Debug.dumpLivenessAnalysisIntermediate),
	     ("dump-liveness-analysis-context",
	      Debug.dumpLivenessAnalysisContext),
	     ("dump-liveness-analysis-result",
	      Debug.dumpLivenessAnalysisResult),
	     ("dump-dead-code-elimination-result",
	      Debug.dumpDeadCodeEliminationResult),
	     ("dump-target",
	      Debug.dumpTarget)]

	fun checkBooleanSwitches (s, (name, switch)::rest) =
	    if "--" ^ name = s then (switch := true; true)
	    else if "--no-" ^ name = s then (switch := false; true)
	    else checkBooleanSwitches (s, rest)
	  | checkBooleanSwitches (_, nil) = false

	fun options ("--rtt-level=no"::rest) =
	    (Bootstrap.rttLevel := Bootstrap.NO_RTT;
	     options rest)
	  | options ("--rtt-level=core"::rest) =
	    (Bootstrap.rttLevel := Bootstrap.CORE_RTT;
	     options rest)
	  | options (s::rest) =
	    if checkBooleanSwitches (s, booleanSwitches) then options rest
	    else s::rest
	  | options nil = nil

	fun defaults () = () (* override defaults from MakeSwitches here *)

	fun stoc arguments =
	    (defaults (); stoc' (options arguments))
	    handle Error.Error (_, _) => OS.Process.failure
		 | Crash.Crash message =>
		       (TextIO.output (TextIO.stdErr,
				       "CRASH: " ^ message ^ "\n");
			OS.Process.failure)
		 | e =>   (*--**DEBUG*)
		       (TextIO.output (TextIO.stdErr,
				       "uncaught exception " ^
				       exnName e ^ "\n");
			OS.Process.failure)
    end
