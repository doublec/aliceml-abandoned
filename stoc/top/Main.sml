functor Main   (structure Error: ERROR
			  where type position = Source.position

		structure Grammar_Core: GRAMMAR_CORE
					where type Info = Source.position
		structure Grammar_Module: GRAMMAR_MODULE
					where Core = Grammar_Core
					where type Info = Source.position
		structure Grammar_Program: GRAMMAR_PROGRAM
					where Module = Grammar_Module
					where type Info = Source.position

		structure Parse: PARSE  where Grammar = Grammar_Program)
:> MAIN =
  struct

    (* Printing results *)

    fun input () =  TextIO.inputLine(TextIO.stdIn)
    fun output s = (TextIO.output(TextIO.stdOut, s);
		    TextIO.flushOut(TextIO.stdOut) )

    fun ok() = output "OK\n"

    fun error(I, message, source) =
	let
	    val (pos1,pos2) = I
	    val a = Int.toString pos1
	    val b = Int.toString pos2
	in
	    output(a ^ "-" ^ b ^ ": " ^ message ^ "\n")
	end
(*	let
	    val {sourceline,line,column,height,width}= Source.getLine(I,source)

	    val sourceline = if line = 0 then ""
			     else if height > 1 then sourceline ^ " ...\n"
			     else sourceline ^ "\n"
	    val width      = if width = 0 then 1
			     else if height > 1 then
				String.size(sourceline) - column
			     else width
	    val underline  = if line = 0 then "" else 
			     repeat(column-1,#" ") ^ repeat(width,#"^") ^ "\n"
	    val position   = if Source.countLines(source) = 1 then ""
			     else if line = 0 then "Unknown position: "
			     else if height = 1 then
				"Line " ^ Int.toString line ^ ": "
			     else
				"Lines " ^ Int.toString line ^
				     "-" ^ Int.toString(line+height-1) ^ ": "
	in
	    output(sourceline ^ underline ^ position ^ message ^ "\n")
	end
*)
    fun crash(s) = output("CRASH in " ^ s ^ "\n")


    fun process f source =
	    ( f(source^";") ; ok() )
	    handle Error.ERROR(I, s) => error(I, s, source)
	         | Crash.CRASH(s)    => crash(s)


    (* Parsing only *)

    fun parseString source = process Parse.parse source


    (* Checking a program file *)

    fun parseFile name =
	let
	    val file   = TextIO.openIn name
	    val source = TextIO.inputAll file
	    val _      = TextIO.closeIn file
	in
	    process Parse.parse source
	end

  end
