(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

functor MakeEngine(val cmd: unit -> string * string list
		   structure Code: CODE) :> ENGINE where type code = Code.t =
    struct
	type t = Unix.proc option ref
	type code = Code.t
	type value = int

	exception Format of string

	val valueToString = Int.toString

	fun start () = ref (SOME (Unix.execute (cmd ())))

	fun instream proc = #1 (Unix.streamsOf (valOf (!proc)))
	fun outstream proc = #2 (Unix.streamsOf (valOf (!proc)))

	fun stop proc =
	    (TextIO.closeOut (outstream proc);
	     proc := NONE)

	datatype arg =
	    CODE of Code.t
	  | VALUE of value
	  | STRING of string

	fun sendCommand (proc, command, args) =
	    let
		val q = outstream proc
	    in
		TextIO.output (q, "Command: " ^ command ^ "\n");
		List.app (fn arg =>
			  (TextIO.output (q, "Argument: ");
			   case arg of
			       CODE code =>
				   Code.externalize (q, code)
			     | VALUE value =>
				   TextIO.output (q, valueToString value)
			     | STRING string =>
				   TextIO.output (q, string);
			   TextIO.output1 (q, #"\n"))) args;
		TextIO.output1 (q, #"\n");
		TextIO.flushOut q
	    end

	local
	    fun split (#":"::cs) = (nil, cs)
	      | split (c::cs) =
		let
		    val (name, value) = split cs
		in
		    (c::name, value)
		end
	      | split nil = raise Format "split: colon expected"
	in
	    fun parseResult proc =
		case TextIO.inputLine (instream proc) of
		    "\n" => nil
		  | s =>
			let
			    val (name, value) = split (String.explode s)
			in
			    (String.implode name, String.implode value)::
			    parseResult proc
			end
	end

	fun buildFunctor proc code =
	    (sendCommand (proc, "buildFunctor", [CODE code]);
	     case parseResult proc of
		    [("Result", s)] => valOf (Int.fromString s)
		  | _ => raise Format "buildFunctor: result expected")

	fun saveValue proc filename value =
	    (sendCommand (proc, "saveValue", [STRING filename, VALUE value]);
	     case parseResult proc of
		 [("Result", _)] => ()
	       | _ => raise Format "saveValue: result expected")
    end
