structure Main :> MAIN =
    struct

	val inFile = ref ""


	fun printEx s = (TextIO.output (TextIO.stdErr, s);
			 TextIO.flushOut TextIO.stdErr;
			 OS.Process.exit OS.Process.failure)


	fun handleEx ErrorMsg.Error = printEx ""
	  | handleEx exn =
	    printEx ("Jacke: unhandled internal exception: "
		     ^ General.exnName exn ^ "\n")


	fun usage () = printEx "Usage: jacke infile\n"

	    
	fun start process = (process(); OS.Process.success)


	fun jacke () = Output.output (!inFile)


	fun setFile infile = inFile := infile


	fun main' [infile]                = (setFile infile;
					     start jacke)
	  | main' _                       = usage () 


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
	    printEx ("Jacke: input file '" ^ !inFile ^ "' does not exist\n")
	     | (IO.Io {name,function="openOut",cause}) =>
	    printEx "Jacke: output file not accessible\n"
	     | (IO.Io {name,function="inputAll",cause}) =>
	    printEx "Jacke: input file seems to be a directory\n"
	     | exn => handleEx exn
       
    end
