structure Main : MAIN =
    struct
	val version = "0.1"
	    

	fun usage() = (TextIO.output (TextIO.stdErr,"Usage: hose infile [-o outfile]\n");
		       TextIO.flushOut TextIO.stdErr;
		       OS.Process.failure)

	    
	fun start process =
	    (TextIO.print("Hose "^version^" tries to run\n");
	     process();
	     TextIO.print "\n";
	     OS.Process.success)


	fun main' [infile, "-o", outfile] = (inFile := infile; outFile := outfile; start hose) 
	  | main' ["-o", outfile, infile] = (inFile := infile; outFile := outfile; start hose)
	  | main' [infile]                = (inFile := infile; start hose)
	  | main' _                       = usage () 
	    
	fun main() =
	    let
		val homeDir = case OS.Path.dir(CommandLine.name())
		    of ""  => OS.Path.currentArc
		  | dir => dir
	    in
		Sml.basisPath := OS.Path.joinDirFile{dir=homeDir, file="basis"};
		OS.Process.exit(main'(CommandLine.arguments()))
	    end
	handle exn =>
	    (TextIO.output(TextIO.stdErr, "Hose: unhandled internal exception " ^ General.exnName exn ^ "\n");
	     OS.Process.exit OS.Process.failure)
    end
