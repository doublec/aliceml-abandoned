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
			Collect.collect (Parse.parse input)
		    end

		val lexMap  = Extract.extract lexList 

		val autoMap = Table.makeAuto lexMap

		val out = TextIO.openOut (!outFile)

	    in	    
		Output.printLexList(out, lexList, autoMap);
		TextIO.closeOut out
	    end



	(* from HaMLet *)

	val version = "0.1"
	    

	fun usage() = (TextIO.output (TextIO.stdErr,"Usage: hose infile [-o outfile]\n");
		       TextIO.flushOut TextIO.stdErr;
		       OS.Process.failure)

	    
	fun start process = (TextIO.print("Hose "^version^" tries to run\n");
			     process();
			     TextIO.print "\n";
			     OS.Process.success)


	fun main' [infile, "-o", outfile] = (inFile := infile; outFile := outfile; start hose) 
	  | main' ["-o", outfile, infile] = (inFile := infile; outFile := outfile; start hose)
	  | main' [infile]                = (inFile := infile; outFile := infile ^ ".sml"; start hose)
	  | main' _                       = usage () 
	    
	fun main() = OS.Process.exit(main'(CommandLine.arguments()))
	    handle exn =>
		(TextIO.output(TextIO.stdErr, "Hose: unhandled internal exception " ^ General.exnName exn ^ "\n");
		 OS.Process.exit OS.Process.failure)
       
    end
