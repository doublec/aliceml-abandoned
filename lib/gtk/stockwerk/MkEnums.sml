(*
 * Authors:
 *   Robert Grabowski <grabow@ps.uni-sb.de>
 *
 * Copyright:
 *   Robert Grabowski, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

(*
  This functor generates the enums component (structure and signature).
*)


functor MkEnums(structure TypeManager : TYPE_MANAGER
	        structure Special : SPECIAL
	        val space : Util.spaces) :> GENERATOR =
    struct
	open TypeTree
	open TypeManager

	val safeName = Util.moduleName(space)^"Enums"

	(* Indentation constants *)
	val sigIndent = Util.indent 1
	val wrIndent = Util.indent 2

	val siginfo = {
	     name = Util.strUpper(safeName)^"-sig.aml" ,
	     intro = 
	         ["(* This is a generated file. ",
		  "Modifications may get lost. *)\n\n",
		  "signature ", Util.strUpper(safeName), " =\n",
		  "sig\n\n"] ,
	      outro =
		 ["end\n\n"]
	    } : Util.fileInfo

	val wrapperinfo = {
             name = safeName^".aml" ,
             intro =
		["(* This is a generated file. ",
		 "Modifications may get lost. *)\n\n",
		 "import signature ", Util.strUpper(safeName), 
		 " from \"", Util.strUpper(safeName), "-sig\"\n\n",
		 "structure ", safeName, " :> ",Util.strUpper(safeName)," =\n",
		 Util.indent 1, "struct\n\n"] ,
	     outro = 
		 [Util.indent 1, "end\n\n"]
            } : Util.fileInfo


	fun sigEntry (name, members) =
	let
	    val mt = Util.makeTuple " | " ""
	    val memnames = map (fn (name,_) => name) members
	in
	   [sigIndent,"datatype ",name," = ",mt memnames,"\n",
	    sigIndent,"val ",name,"ToInt : ",name," -> int\n",
	    sigIndent,"val IntTo",name," : int -> ",name,"\n\n"]
	end

	fun wrapperEntry (name, members) =
	let
	    val mt = Util.makeTuple " | " ""
	    val memnames = map (fn (name,_) => name) members
	    val e2r = map (fn (name,num) => name^" => "^num) members
	    val r2e = (map (fn (name,num) => num^" => "^name)
		       (Util.removeDuplicates (fn (x,y) => #2x = #2y) members))
		   @(if null members then nil else ["_ => "^(#1(hd members))])
	in
	   [wrIndent,"datatype ",name," = ",mt memnames,"\n",
	    wrIndent,"val ",name,"ToInt = fn ",mt e2r,"\n",
	    wrIndent,"val IntTo",name," = fn ",mt r2e,"\n\n"]
	end

	(* SIGNATURE AND WRAPPER ENTRIES *)
	fun processItem (ENUM (name,members)) = 
	let
        
	    val members' = map (fn (name,num) => 
				(Util.computeEnumName (space,name), 
				LargeInt.toString num)) 
		               (List.filter checkEnumMember members)
	in
	    if null members' 
		then ( nil, nil )
		else ( sigEntry (name,members'), wrapperEntry (name,members') )
	end
	  | processItem _ = ( nil , nil )

        (* main function for creating enums files *)
    fun create tree =
        let
            fun testName d = declName d <> ""
            
	        val _ = print (Util.separator("Generating "^safeName))
	        val myItems = Util.filters [Util.funNot Special.isIgnored, 
                       testName, isItemOfSpace space, checkItem] tree

	        val s = Util.openFile siginfo
	        val w = Util.openFile wrapperinfo

	        val psl = Util.outputStrings
	        fun dpsl (sl, wl) = (psl s sl ; psl w wl )
	    in
	        ( List.app (dpsl o processItem) myItems ;
	          Util.closeFile s ;
	          Util.closeFile w )
	    end

    end

