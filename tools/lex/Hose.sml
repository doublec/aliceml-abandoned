structure Hose :> HOSE =
    struct

	val inFile = ref ""
	val outFile = ref ""


	fun hose () =
	    let 
		    
		val lexList =
		    let
			val inputStream = TextIO.openIn (!inFile)
			val input = TextIO.inputAll inputStream
			val _ = TextIO.closeIn inputStream
		    in
			Collect.collect (Parse.parse input, !inFile)
		    end

		val lexMap  = Extract.extract (lexList, !inFile)

		val autoMap = Table.makeAuto lexMap

		val out = TextIO.openOut (!outFile)

	    in	    
		Output.printLexList(out, lexList, autoMap);
		TextIO.closeOut out
	    end



	(* from HaMLet *)

	fun usage() = (TextIO.output (TextIO.stdErr,"Usage: hose infile [-o outfile]\n");
		       TextIO.flushOut TextIO.stdErr;
		       OS.Process.failure)

	    
	fun start process = (process(); OS.Process.success)


	fun main' [infile, "-o", outfile] = (inFile := infile; outFile := outfile; start hose) 
	  | main' ["-o", outfile, infile] = (inFile := infile; outFile := outfile; start hose)
	  | main' [infile]                = (inFile := infile; outFile := infile ^ ".sml"; start hose)
	  | main' _                       = usage () 
	    
	fun main() = OS.Process.exit(main'(CommandLine.arguments()))
	    handle (IO.Io {name,function="openIn",cause}) => (TextIO.output (TextIO.stdErr, "input file does not exist\n");
			   TextIO.flushOut TextIO.stdErr;
			   OS.Process.exit OS.Process.failure)
		 | (IO.Io {name,function="openOut",cause}) => (TextIO.output (TextIO.stdErr, "invalid output file\n");
			   TextIO.flushOut TextIO.stdErr;
			   OS.Process.exit OS.Process.failure)
		 | (IO.Io {name,function="inputAll",cause}) => (TextIO.output (TextIO.stdErr, "input file seems to be a directory\n");
			   TextIO.flushOut TextIO.stdErr;
			   OS.Process.exit OS.Process.failure)
		 | exn => (TextIO.output(TextIO.stdErr, "Hose: unhandled internal exception: " ^ General.exnName exn ^ "\n");
			   OS.Process.exit OS.Process.failure)
       
    end
