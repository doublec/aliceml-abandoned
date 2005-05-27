(* ADG 1.0 - Alice Dependence Graph
*
*  Author: Sebastian Germesin
*
*  $Revision$
*
*  Last updated: $Date$ by $Author$
* 
*
*)

changequote([[,]])

ifdef([[gdl]],[[import structure Backend from "GDL"]],
[[import structure Backend from "DOT"]])


val head = 
    "// Alice Dependence Graph           //\n" ^ 
    "//                                  //\n" ^ 
    "// Author: Sebastian Germesin       //\n" ^ 
    "//                                  //\n" ^
    "// eMail: germi@ps.uni-sb.de        //\n" ^ 
    "//                                  //\n" ^
    "//////////////////////////////////////\n" ^
    "//\n" ^ 
    "//\n" ^ 
    "// written in " ^ Backend.name ^ "\n" ^
    "// Annotations:\n" ^
    "//\n" ^ 
    "// for looking the graph:\n" ^
    "//     use " ^ Backend.program ^ "\n" ^
    "//\n" ^
    "// red box    with black border = main file\n" ^
    "// green box  with black border = built files\n" ^
    "// blue box   with black border = alice-lib files\n" ^
    "// yellow box with black border = unsafe files\n\n\n" ^
    Backend.header ^ 
    "\n\n\n\n"

val usage = "unrecognized option\n\n" ^ 
	    "Usage: \nalicerun Main <input_filename> " ^
	    "(-{include, exclude} regex)* \n" ^ 
	    "              [-out <output_filename>]\n"

fun printCluding [] = ()
  | printCluding ((true, x) :: xs)  = 
		  (TextIO.output (TextIO.stdOut, 
				  "including files matching " ^ x ^ "\n");
		   printCluding xs)
  | printCluding ((false, x) :: xs) = 
		  (TextIO.output (TextIO.stdOut, 
				  "excluding files matching " ^ x ^ "\n");
		   printCluding xs)

fun start (inFile, regex, outFile) = 
    let
	val outStream = TextIO.openOut (outFile ^ Backend.fileEnding)
    in 
	TextIO.output (TextIO.stdOut, "input  file: " ^ inFile ^ "\n");
	TextIO.output (TextIO.stdOut, "output file: " ^ outFile ^ 
				      Backend.fileEnding ^ "\n");
	printCluding regex;
	TextIO.output (outStream, head);
	Backend.output (inFile, regex, outStream);
	TextIO.closeOut outStream;
	OS.Process.exit OS.Process.success
    end

val infile  = ref ""
val outfile = ref "output"
val regex   = ref nil (* (bool * string) list *)

fun parse       []                = start (!infile, rev (!regex), !outfile)
  | parse ("-out" :: x :: xs)     = (outfile := x; parse xs)
  | parse ("-include" :: x :: xs) = (regex := (true, x) :: !regex; parse xs)
  | parse ("-exclude" :: x :: xs) = (regex := (false, x) :: !regex; parse xs)
  | parse       _                 = (TextIO.output (TextIO.stdErr, usage);
				     OS.Process.exit OS.Process.failure)

val _  = case CommandLine.arguments () of
	         []        => (TextIO.output (TextIO.stdErr, usage);
			       OS.Process.exit OS.Process.failure)
	       | (x :: xs) => (infile := x; parse xs) 

