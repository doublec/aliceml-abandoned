(*
 * Authors:
 *   Sven Woop <woop@ps.uni-sb.de>
 *
 * Copyright:
 *   Sven Woop, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

local
    open Util
in

    (* Prüft ob eine Option in der Komandozeile vorhanden *)

    fun is_option_set opt nil = false
      | is_option_set opt (s::sr) = 
	if s = opt then true else is_option_set opt sr
	    
    (* Liefert den Wert einer Option zurück. Bsp.: "-i on" wird zu SOME "on" *)
	    
    fun get_option_value opt nil = NONE
      | get_option_value opt [_] = NONE
      | get_option_value opt (s::name::sr) = 
	if s = opt then SOME name 
	else get_option_value opt (name::sr)
	    
    (* "-h a -h b" wird zu [a,b] *)
	    
    fun get_option_list opt nil = nil
      | get_option_list opt [_] = nil
      | get_option_list opt (s::name::xr) = 
	if s = opt then name :: (get_option_list opt xr)
	else get_option_list opt (name::xr)

    (* Gibt die Hilfe aus *)

    fun printHelp() =     
	print("\nAlice-C Binding Generator\n\n"^
	      "Usage: sml @SMLload=Binding [options]\n\n"^
	      "Options are:\n"^
	      "--help:          This helpscreen\n"^
	      "--create-types : Creates the Basic C-Binding c.asig and c.cc\n"^
	      "                 which es needed for each Binding.\n"^
	      "-h file        : The Headerfile to work on.\n"^
	      "-i file        : Additional include file for the binding.\n\n")
	

    (* Hauptfunktion *)
	
    fun run _ =
	let 
	    (* Programmargumente *)
	    val args = SMLofNJ.getArgs()
		
	    val config = case get_option_list "-c" args of
		             nil => "config.xml"
			   |(x::xr) => x
	in
	    if is_option_set "--help" args then
		printHelp()

	    else if is_option_set "--create-types" args then
		(Binding.createBasicBinding "C")

	    else if length(get_option_list "-h" args) <> 0 then
		let val header = get_option_value "-h" args
		in case header of
		    NONE => raise Error "Headerfile missing."
		  |(SOME h) => (Config.readCFGFile config;
				Binding.create h (h::(get_option_list "-i" args)))
		end
	    else
		printHelp();

	    OS.Process.exit OS.Process.success
	end

    handle (Error s) => (print ("Error: "^s^"\n");
			 OS.Process.exit OS.Process.failure)

	 | (Warning s) => (print ("Error: "^s^"\n");
			   OS.Process.exit OS.Process.failure)

	 | _ => (print "Error: Unknown Error\n";
		 OS.Process.exit OS.Process.failure)
	
    fun compile() = SMLofNJ.exportFn("Binding", run)

end

