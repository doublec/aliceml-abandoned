structure Main :> MAIN =
    struct

    val binName     = "aliceyacc"

	val inFile      = ref ""
    val outFile     = ref ""
    val smlMode     = ref false


	fun printEx s = (TextIO.output (TextIO.stdErr, s);
			 TextIO.flushOut TextIO.stdErr;
			 OS.Process.exit OS.Process.failure)


	fun handleEx ErrorMsg.Error = printEx ""
	  | handleEx exn =
	    printEx (binName ^ ": unhandled internal exception: "
		     ^ General.exnName exn ^ "\n")


	fun usage () = 
        printEx ("Usage: " ^ binName ^ "[<option>] infile [-o <output file>]\n" ^
                 "Options:\n" ^ 
                 "\t--sml-mode\n" ^
                 "\t\tDo not emit alice style import declarations.\n\n" ^ 
                 "Report bugs using our online bug-tracking system:\n" ^
                 "http://www.ps.uni-sb.de/alice/bugzilla\n")

	    
	fun start process = (process(); OS.Process.success)


	fun jacke () = Output.output (!inFile, !smlMode, !outFile)


	fun setFile infile  = inFile := infile
    fun setOutFile outF = outFile := outF

    fun outExt () = if !smlMode then ".sml" else ".aml" 

	fun main' ("--sml-mode" :: xs)      = (smlMode := true; main' xs)
      | main' [infile]                  = (setFile infile;
					                       setOutFile (infile ^ outExt ());
                                           start jacke)
      | main' [infile, "-o", outfile]   = (setFile infile;
                                           setOutFile outfile;
                                           start jacke)
	  | main' _                         = usage () 


	fun main () =
	    let
		(* ugly hack to circumvent the problem that "jacke-image"
		 * is sometimes an element of the CommandLine.arguments()
		 * (Windows)
		 *)
		val args = case CommandLine.arguments() of
		    ("jacke-image"::xs) => xs
		  | xs                  => xs
	    in
		OS.Process.exit(main' args)
	    end
	handle (IO.Io {name,function="openIn",cause}) =>
	    printEx (binName ^ ": input file '" ^ !inFile ^ "' does not exist\n")
	     | (IO.Io {name,function="openOut",cause}) =>
	    printEx (binName ^ ": output file not accessible\n")
	     | (IO.Io {name,function="inputAll",cause}) =>
	    printEx (binName ^ ": input file seems to be a directory\n")
	     | exn => handleEx exn
       
    end
