
functor MkCore(structure TypeManager : TYPE_MANAGER
	       structure Special : SPECIAL
	       val space : Util.spaces
	       val tree : TypeTree.tree) :> GENERATOR =
    struct
	open TypeTree
	open TypeManager

	val unsafeName = "Unsafe"^Util.spaceName(space)
	val safeName = Util.spaceName(space)^"Core"

	(* Indentation constants *)
	val sigIndent = Util.indent 1
	val wrIndent = Util.indent 2

	val siginfo = {
	     name = Util.strUpper(safeName)^"-sig.aml" ,
	     intro = 
	         ["(* This is a generated file. ",
		  "Modifications may get lost. *)\n\n",
		  "import structure GtkUtils from \"GtkUtils\"\n",
		  "import structure GtkEnums from \"GtkEnums\"\n",
		  "import structure GdkEnums from \"GdkEnums\"\n",
		  "\n",
		  "signature ", Util.strUpper safeName, " =\n",
		  "sig\n",
		  sigIndent, "type object = GtkUtils.object\n" ,
		  sigIndent, "exception TypeMismatch of string\n",
		  sigIndent, "\n"] ,
	      outro =
		 ["end\n\n"]
	    } : Util.fileInfo

	val wrapperinfo = {
             name = safeName^".aml" ,
             intro =
		["(* This is a generated file. ",
		 "Modifications may get lost. *)\n\n",
		 "import structure ",unsafeName," from \"",unsafeName, "\"\n",
		 "import structure GtkUtils from \"GtkUtils\"\n",
		 "import structure GtkEnums from \"GtkEnums\"\n",
		 "import structure GdkEnums from \"GdkEnums\"\n",
		 "import signature ", Util.strUpper(safeName), 
		 " from \"", Util.strUpper(safeName), "-sig\"\n\n",
		 "structure ", safeName, " :> ",Util.strUpper(safeName)," =\n",
		 Util.indent 1, "struct\n",
		 wrIndent, "type object = GtkUtils.object\n",
		 wrIndent, "exception TypeMismatch = GtkUtils.TypeMismatch",
		 "\n\n"],
	     outro = 
		 [Util.indent 1, "end\n\n"]
            } : Util.fileInfo

	(* SIGNATURE CODE GENERATION *)
	fun sigEntry(funName, ret, arglist, doinout) =
        let
	    fun convEnum (ENUMREF n)= Util.spaceName(getEnumSpace n)^"Enums."^n
	      | convEnum (TYPEREF (_,t)) = convEnum t
	      | convEnum t               = getAliceType t
	    val wname = Util.computeWrapperName(space,funName)^
		          (if doinout then "'" else "")
	    val aType = getAliceFunType (wname,ret,arglist,doinout) convEnum
	    val cType = getCFunType(funName,ret,arglist,true)
	in
	    [sigIndent, "(* ", cType, " *)\n",
	     sigIndent, aType, "\n\n"]
	end

	(* WRAPPER CODE GENERATION *)
	fun wrapperEntry(funName, ret, arglist, doinout) =
	let
	    val wname = Util.computeWrapperName(space,funName) ^
		        (if doinout then "'" else "")
	    val argTypes = map removeTypeRefs (ret::(map (fn i=> #3i) arglist))
	    val generateSimple =
		not (List.exists (fn (ENUMREF _)=> true | _=> false) argTypes)
	    val (ins, outs') = splitInOuts (arglist, doinout)
	    val outs = if ret = VOID then outs' else (OUT,"ret",ret)::outs'
	    fun convEnum toReal (_,vname, t) =
	       (case removeTypeRefs t of
		    ENUMREF ename =>
			Util.spaceName(getEnumSpace ename)^"Enums."^
			    (if toReal then ename^"ToReal" else "RealTo"^ename)
			    ^" "^vname
	         | _ => vname)
	in
	    if generateSimple
	    then
		[wrIndent, "val ", wname, " = ", 
		 unsafeName, ".", wname, "\n"]
	    else
		[wrIndent, "fun ", wname, "(", 
		 Util.makeTuple ", " "" (map (fn info => #2info) ins), 
		 ") =\n",
		 wrIndent, wrIndent, "let val (",
		 Util.makeTuple ", " "x" (map (fn info => #2info) outs), 
		 ") = ", unsafeName, ".", wname, "(",
		 Util.makeTuple ", " "" (map (convEnum true) ins), ")\n", 
		 wrIndent, wrIndent, "in (",
		 Util.makeTuple ", " "x" (map (convEnum false) outs), ")\n",
		 wrIndent, wrIndent, "end\n"]
	end	

	(* SIGNATURE AND WRAPPER ENTRIES *)
	fun processItem (FUNC (funName,ret,arglist)) = 
	let
	    val al =  splitArgTypes arglist
	    fun call f = f(funName, ret, al, false) @
		         (if numOuts (al,false) > 0 
			      then f(funName,ret,al,true)
			      else nil)
	in
	    ( call sigEntry , call wrapperEntry )
	end
	  | processItem _ = ( nil , nil )

        val myItems' = List.filter (Util.funNot Special.isIgnored) tree
	val myItems = Util.filters [isItemOfSpace space, checkItem,
				    Util.funNot Special.isIgnoredSafe] 
		      (myItems' @ Special.changedFuns @ Special.specialFuns)

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
