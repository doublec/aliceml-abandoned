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
  This functor generates the unsafe component (structure and signature).
*)


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
		  "import structure Core from \"Core\"\n",
	          "import structure GnomeCanvasEnums ",
		    "from \"GnomeCanvasEnums\"\n",
		  "import structure GtkEnums from \"GtkEnums\"\n",
		  "import structure GdkEnums from \"GdkEnums\"\n",
		  "import structure PangoEnums from \"PangoEnums\"\n",
		  "\n",
		  "signature ", Util.strUpper unsafeName, " =\n",
		  "sig\n",
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
		 "import structure Core  from \"Core\"\n",
	          "import structure GnomeCanvasEnums ",
		    "from \"GnomeCanvasEnums\"\n",
		 "import structure GtkEnums from \"GtkEnums\"\n",
		 "import structure GdkEnums from \"GdkEnums\"\n",
		 "import structure PangoEnums from \"PangoEnums\"\n",
		 "import signature ", Util.strUpper(unsafeName), 
		 " from \"", Util.strUpper(unsafeName), "-sig\"\n\n",
		 "structure ", unsafeName, " :> ",
		 Util.strUpper unsafeName, " =\n",
		 Util.indent 1, "struct\n",
		 wrIndent, "open GnomeCanvasEnums\n",
		 wrIndent, "open GtkEnums\n",
		 wrIndent, "open GdkEnums\n",
		 wrIndent, "open PangoEnums\n",
		 "\n"],
	     outro = 
		 [Util.indent 1, "end\n\n"]
            } : Util.fileInfo


	(* SIGNATURE CODE GENERATION *)
	fun sigEntry(funName, ret, arglist, doinout) =
        let
	    val wname = Util.computeWrapperName(space,funName)^
		          (if doinout then "'" else "")
	    val aType = getAliceFunType(wname,ret,arglist,doinout) getAliceType
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
	    val (ins, outs') = splitInOuts (arglist, doinout)
	    val outs = if ret = VOID then outs' else (OUT,"ret",ret)::outs'
	    val insConv = 
		map (fn (_,vname,t)=>safeToUnsafe vname (removeTypeRefs t)) ins
	    val outsConv = 
		map (fn (_,vname,t)=>unsafeToSafe vname (removeTypeRefs t))outs
	    fun noConv (_,vname,_) = vname
            val generateSimple = (insConv@outsConv) = map noConv (ins@outs)
	in
	    if generateSimple then
		[wrIndent, "val ", wname, " = ", nativeName, ".", wname, "\n"]
	    else
		[wrIndent, "fun ", wname, "(", 
		 Util.makeTuple ", " "" (map (fn info => #2info) ins), 
		 ") =\n",
		 wrIndent, wrIndent, "let val (",
		 Util.makeTuple ", " "x" (map (fn info => #2info) outs), 
		 ") = ", nativeName, ".", wname, "(",
		 Util.makeTuple ", " "" insConv, ")\n", 
		 wrIndent, wrIndent, "in (",
		 Util.makeTuple ", " "x" outsConv, ")\n",
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
	    val myItems = Util.filters [isItemOfSpace space, checkItem] 
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
