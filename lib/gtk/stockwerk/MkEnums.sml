
functor MkEnums(structure TypeManager : TYPE_MANAGER
	        structure Special : SPECIAL
	        val space : Util.spaces
	        val tree : TypeTree.tree) :> GENERATOR =
    struct
	open TypeTree
	open TypeManager

	val safeName = Util.spaceName(space)^"Enums"

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
	    "(**)",sigIndent,"val ",name,"ToReal : ",name," -> real\n",
	    "(**)",sigIndent,"val RealTo",name," : real -> ",name,"\n\n"]
	end

	fun wrapperEntry (name, members) =
	let
	    val mt = Util.makeTuple " | " ""
	    val memnames = map (fn (name,_) => name) members
	    val e2r = map (fn (name,num) => name^" => "^num^".0") members
	    val r2e = map (fn (name,num) => num^".0 => "^name)
		       (Util.removeDuplicates (fn (x,y) => #2x = #2y) members)
	in
	   [wrIndent,"datatype ",name," = ",mt memnames,"\n",
	    wrIndent,"val ",name,"ToReal = fn ",mt e2r,"\n",
	    wrIndent,"val RealTo",name," = fn ",mt r2e,"\n\n"]
	end

	(* SIGNATURE AND WRAPPER ENTRIES *)
	fun processItem (ENUM (name,members)) = 
	let
(*	    val comment = ["\n", sigIndent, "(* ", name, " *)\n"]
	    fun isTooBig n = Int.maxInt <> NONE andalso 
		             LargeInt.fromInt(valOf(Int.maxInt)) < n
	    fun sigEntry (mem,num) = 
		if isTooBig num then ["(* ", mem, " is too big *)\n"] else
		[sigIndent,"val ",Util.computeEnumName (space,mem), " : int\n"]
	    fun wrapperEntry (mem, num) = 
		if isTooBig num then nil else
		[wrIndent, "val ",Util.computeEnumName (space,mem), " = ", 
		 LargeInt.toString num, "\n"]*)
	    val members' = map (fn (name,num) => 
				(Util.computeEnumName (space,name), 
				LargeInt.toString num)) members
	in
(*	    ( List.concat (comment::(map sigEntry members)) , 
	      List.concat (map wrapperEntry members) 
	    )*)
	    if null members 
		then ( nil, nil )
		else ( sigEntry (name,members'), wrapperEntry (name,members') )
	end
	  | processItem _ = ( nil , nil )

	val myItems = Util.filters [Util.funNot Special.isIgnored, 
				    isItemOfSpace space, checkItem,
				    Util.funNot Special.isIgnoredSafe] tree

        (* main function for creating safe files *)
        fun create() =
	let
	    val _ = print ("Generating "^safeName^"\n")
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

