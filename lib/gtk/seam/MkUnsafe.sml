
functor MkUnsafe(structure TypeManager : TYPE_MANAGER
	         structure Special : SPECIAL
	         val space : Util.spaces) :> GENERATOR =
    struct
	open TypeTree
	open TypeManager

	val nativeName = "Native"^Util.spaceName(space)
	val unsafeName = Util.spaceName(space)^"Unsafe"

	(* Indentation constants *)
	val sigIndent = Util.indent 1
	val wrIndent = Util.indent 2

	val siginfo = {
	     name = Util.strUpper(unsafeName)^"-sig.aml" ,
	     intro = 
	         ["(* This is a generated file. ",
		  "Modifications may get lost. *)\n\n",
		  "import structure GtkTypes from \"GtkTypes\"\n",
		  "import structure GtkEnums from \"GtkEnums\"\n",
		  "import structure GdkEnums from \"GdkEnums\"\n",
		  "import structure PangoEnums from \"PangoEnums\"\n",
		  "\n",
		  "signature ", Util.strUpper unsafeName, " =\n",
		  "sig\n",
		  "(**)", sigIndent, "type object = GtkTypes.object\n" ,
		  "(**)", sigIndent, "type arg = GtkTypes.arg\n",
		  sigIndent, "\n"] ,
	      outro =
		 ["end\n\n"]
	    } : Util.fileInfo

	val wrapperinfo = {
             name = unsafeName^".aml" ,
             intro =
		["(* This is a generated file. ",
		 "Modifications may get lost. *)\n\n",
		 "import structure ",nativeName," from \"",nativeName, "\"\n",
		 "import structure GtkTypes from \"GtkTypes\"\n",
		 "import structure GtkCore  from \"GtkCore\"\n",
		 "import structure GtkEnums from \"GtkEnums\"\n",
		 "import structure GdkEnums from \"GdkEnums\"\n",
		 "import structure PangoEnums from \"PangoEnums\"\n",
		 "import signature ", Util.strUpper(unsafeName), 
		 " from \"", Util.strUpper(unsafeName), "-sig\"\n\n",
		 "structure ", unsafeName, " :> ",
		 Util.strUpper unsafeName, " =\n",
		 Util.indent 1, "struct\n",
		 wrIndent, "type object = GtkTypes.object\n",
		 wrIndent, "type arg = GtkTypes.arg\n\n",
		 wrIndent, "open GtkEnums\n",
		 wrIndent, "open GdkEnums\n",
		 wrIndent, "open PangoEnums\n",
		 "\n"],
	     outro = 
		 [Util.indent 1, "end\n\n"]
            } : Util.fileInfo


        local
	    val enumDecls = ref nil
	    fun addToList n = if not (Util.contains n (!enumDecls))
		                   then (enumDecls := (n::(!enumDecls)) ; true)
                                   else false
	    fun addEnum (ENUMREF n)     = 
		if addToList n then
		    [if space = getEnumSpace n then "(**)" else "", 
		     sigIndent, "type ", n, " = ", 
		     Util.spaceName(getEnumSpace n), "Enums.",n, "\n"] 
		else nil
	      | addEnum (TYPEREF (_,t)) = addEnum t
	      | addEnum _               = nil
	    fun getType (_,_,t) = t
	in
	    fun sigEnumDecls (ret,arglist) = 
		List.concat (map addEnum (ret::(map getType arglist)))
	end

	(* SIGNATURE CODE GENERATION *)
	fun sigEntry(funName, ret, arglist, doinout) =
        let
	    val wname = Util.computeWrapperName(space,funName)^
		          (if doinout then "'" else "")
	    val aType = getAliceFunType(wname,ret,arglist,doinout) getAliceType
	    val cType = getCFunType(funName,ret,arglist,true)
	in
	    sigEnumDecls (ret,arglist) @
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
		not (List.exists (fn (ENUMREF _) => true | 
				     (POINTER _) => true | 
					       _ => false) argTypes)
	    val (ins, outs') = splitInOuts (arglist, doinout)
	    val outs = if ret = VOID then outs' else (OUT,"ret",ret)::outs'
	    fun convArgs toNative (_,vname, t) =
	       (case removeTypeRefs t of
		    ENUMREF ename =>
			(if toNative then ename^"ToInt" else "IntTo"^ename)
			^" "^vname
		  | POINTER t'    =>
			if toNative 
			    then vname 
			    else "GtkCore.addObject "^vname
		  | LIST (_,POINTER t') =>
			if toNative 
			    then vname 
			    else "map GtkCore.addObject "^vname
		  | _ => vname)
	in
	    if generateSimple
	    then
		[wrIndent, "val ", wname, " = ", nativeName, ".", wname, "\n"]
	    else
		[wrIndent, "fun ", wname, "(", 
		 Util.makeTuple ", " "" (map (fn info => #2info) ins), 
		 ") =\n",
		 wrIndent, wrIndent, "let val (",
		 Util.makeTuple ", " "x" (map (fn info => #2info) outs), 
		 ") = ", nativeName, ".", wname, "(",
		 Util.makeTuple ", " "" (map (convArgs true) ins), ")\n", 
		 wrIndent, wrIndent, "in (",
		 Util.makeTuple ", " "x" (map (convArgs false) outs), ")\n",
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
	  | processItem (STRUCT (structName, members)) =
	    let
	        fun call f get = 
		let
		    fun prepare (mname, mtype) =
		    let
			val (funName,ret,arglist) = 
			    makeFieldFun space (structName,mname,mtype,get)
		    in
			f(funName,ret,splitArgTypesNoOuts arglist,false)
		    end
		in
		    List.concat 
		        (map prepare (List.filter checkStructMember members))
		end
	    in
		( List.concat (map (call sigEntry) [true,false]),
		  List.concat (map (call wrapperEntry) [true,false]) )
	    end
	  | processItem (UNION (unionName, members)) =
	      processItem (STRUCT (unionName, members))
	  | processItem _ = ( nil , nil )

        (* main function for creating unsafe files *)
        fun create tree =
	let
	    val _ = print (Util.separator("Generating "^unsafeName))
	    val myItems' = List.filter (Util.funNot Special.isIgnored) tree
	    val myItems = Util.filters [isItemOfSpace space, checkItem,
				        Util.funNot Special.isIgnoredSafe] 
		            (myItems'@Special.changedFuns@Special.specialFuns)

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
