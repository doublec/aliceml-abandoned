
functor MkNative(structure TypeManager : TYPE_MANAGER
		 structure Special : SPECIAL
		 val space : Util.spaces) :> GENERATOR = 
    struct
	open TypeTree
	open TypeManager

	val nativeName = "Native"^Util.spaceName(space)

	(* Indentation constant *)
	val sigIndent = Util.indent 2
	val wrIndent = "  "

        val siginfo = {
 	     name = nativeName^".asig" ,
	     intro =        
 	         ["(* This is a generated file. ",
		  "Modifications may get lost. *)\n\n",
		  "signature ", Util.strUpper nativeName, "_COMPONENT =\n",
		  "sig\n",
		  Util.indent 1, "structure ", nativeName, " :\n",
		  Util.indent 1, "sig\n",
		  sigIndent, "exception TypeMismatch of string\n"] ,
	     outro = 
		 [Util.indent 1, "end\n",
		  "end\n\n"]
            } : Util.fileInfo

	val wrapperinfo = {
             name = nativeName^".cc" ,
	     intro = 
		 ["// This is a generated file. ",
		  "Modifications may get lost.\n\n",
		  "#include \"NativeUtils.hh\"\n"] @
		 (map (fn s => "#include \""^s^"\"\n") Special.includeFiles)@
		 ["\n\n"] ,
             outro = []
            } : Util.fileInfo

        (* SIGNATURE CODE GENERATION *)
	fun sigEntry(funName, ret, arglist, doinout) =
	let
	    val wname = Util.computeWrapperName(space,funName)^
		          (if doinout then "'" else "")
	    val aType = getAliceFunType (wname,ret,arglist,doinout)
		           getAliceNativeType
	    val cType = getCFunType (funName,ret,arglist,true)
	in
	    [sigIndent, "(* ", cType," *)\n",
	     sigIndent, aType, "\n\n"]
	end

	(* WRAPPER CODE GENERATION *)
        fun wrapperEntry(funName, ret, arglist, doinout) =
	let
	    (* Wrapper declaration line *)
	    val wrapperDecl = 
		["// ", getCFunType(funName,ret,arglist,false), 
		 "\nDEFINE", Int.toString(numIns (arglist,doinout)), 
		 "(", nativeName, "_", Util.computeWrapperName(space,funName), 
		 if doinout then "_" else "", ") {\n"]

            (* declaration of input or input/output arguments *)
	    local
		fun inDeclare (NUMERIC(_,false,_))= "DECLARE_INT"
		  | inDeclare (NUMERIC(_,true, _))= "DECLARE_CDOUBLE"
		  | inDeclare (ELLIPSES false)    = "DECLARE_VALIST"
		  | inDeclare BOOL                = "DECLARE_BOOL"
		  | inDeclare (POINTER _)         = "DECLARE_UNMANAGED_POINTER"
		  | inDeclare (STRING _)          = "DECLARE_CSTRING"
		  | inDeclare (FUNCTION _)        = "DECLARE_UNMANAGED_POINTER"
		  | inDeclare (ENUMREF _)         = "DECLARE_ENUM"
		  | inDeclare (TYPEREF (_,t))     = inDeclare t
		  | inDeclare _                   = "DECLARE_UNKNWON";
		val xcounter = ref 0
		fun xcinc () = let val x = Int.toString (!xcounter) 
			       in  (xcounter := !xcounter+1 ; x)
			       end
		val funprefix = fn "GSList" => "g_slist" | _ => "g_list"
	    in
		fun inDeclLine (ARRAY (_,t'),name) =
		        [wrIndent, "DECLARE_CARRAY(", name, ",x", xcinc(), ",",
			 getCType(t'), ",", inDeclare(t'), ");\n"]
		  | inDeclLine (LIST (ctype,t),name) =
			[wrIndent, "DECLARE_GLIST(", name, ",x", xcinc(), ",", 
			 ctype, ",", funprefix ctype, ",", inDeclare t, ");\n"]
		  | inDeclLine (t,name) =
		    let
			val i = xcinc()
			val d = [wrIndent,inDeclare(t),"(",name,",x",i,");\n"]
			val c = if getAliceType t <> "object" then nil else
			        case t of
				    POINTER t' => 
				      [wrIndent,"CHECK_TYPE(",
				       name,",\"",getCType(t'),"\",\"",
 	                               Util.computeWrapperName(space,funName),
				       "\",",i,");\n"]
				  | _ => nil
		    in
			d@c
		    end
	    end

	    (* declaration of output arguments *)
	    local
		fun outInit (NUMERIC _)      = " = 0"
		  | outInit BOOL             = " = false"
		  | outInit (POINTER _)      = " = NULL"
		  | outInit (STRING _)       = " = NULL"
		  | outInit (TYPEREF (_,t))  = outInit t
		  | outInit _                = ""
	    in
		fun outDeclLine (t,name) = 
		        [wrIndent, getCType t, " ", name, outInit t, ";\n"]
	    end  

	    (* all declarations *)
	    local
		fun declare (IN, name, t) = inDeclLine (t,name)
		  | declare (OUT, name, t) =
		    if doinout then inDeclLine(t,name) else outDeclLine(t,name)
	    in
		val allDecls = map String.concat(map declare arglist)
	    end

	    (* C function call *)
	    local
		fun cArgList' (OUT,name,t) = 
		    "reinterpret_cast<"^getCType t^"*>(&"^name^")"
		  | cArgList' (IN,name,t) =
		    let
			fun staticCast t v  = "static_cast<"^t^">("^v^")"
			fun reintCast t v   = "reinterpret_cast<"^t^">("^v^")"
			fun cCast t v       = "("^t^")("^v^")"
			fun noCast _ v      = v
			val cast = case removeTypeRefs t of 
			               ARRAY _    => noCast
			             | STRING _   => reintCast
				     | FUNCTION _ => cCast
				     | _          => staticCast
		    in
			cast (getCType t) name
		    end
		val cArgList = Util.makeTuple ", " "" (map cArgList' arglist)
	    in
		val funCall = ["  ",
			       (case ret of
				   VOID => ""
				 | STRING _ =>"const "^(getCType ret)^" ret = "
				 | _ => (getCType ret)^" ret = "),
				funName, "(", cArgList, ");\n"]
	    end

	    (* return line *)
	    local
	        val alwithret = 
		    if ret = VOID then arglist else (OUT, "ret", ret)::arglist
		val (_,outs) = splitInOuts (alwithret, doinout)

		fun retConv (NUMERIC(_,false,_)) n="Store::IntToWord("^n^")"
		  | retConv (NUMERIC(_,true ,_)) n="Real::New("^n^")->ToWord()"
		  | retConv BOOL       n="BOOL_TO_WORD("^n^")"
		  | retConv(POINTER _) n="Store::UnmanagedPointerToWord("^n^")"
                  | retConv (STRING _) n="String::New(reinterpret_cast<"^
		                         "const char *>("^n^"))->ToWord()"
		  | retConv (LIST(tname,STRING _))n=tname^"ToStringList("^n^")"
		  | retConv (LIST(tname,_))       n=tname^"ToObjectList("^n^")"
		  | retConv (FUNCTION _) n = 
			 "Store::UnmanagedPointerToWord((void*)("^n^"))"
		  | retConv (TYPEREF(_,t)) n = retConv t n
		  | retConv (ENUMREF _) n    = "Real::New("^n^")->ToWord()"
		  | retConv _ n = n
  
		fun makeOutArg (_,name,t) = retConv t name
		val retList = Util.makeTuple ", " "" (map makeOutArg outs)
	    in
		val retLine = wrIndent::
 	           (case length outs of
		       0 => ["RETURN_UNIT;\n"]
		     | 1 => ["RETURN(",retList,");\n"]
		     | i => ["RETURN_TUPLE",Int.toString i,"(",retList,");\n"])
	    end

            (* end line *)
            val endDecl = ["} END\n\n"]

	in
	    wrapperDecl@
	    allDecls@
	    funCall@
	    retLine@
	    endDecl
	end	    

        (* SIGNATURE AND WRAPPER ENTRIES *)
	fun processItem (f as (FUNC (funName,ret,arglist))) =
	let
	    val al = splitArgTypes arglist
	    fun call f = f(funName,ret,al,false) @
		         (if numOuts(al,true) > 0
			      then f(funName,ret,al,true) 
			      else nil)
	    val isspec = (List.exists (fn f' => f=f') Special.specialFuns)
	in
	    ( if isspec then sigEntry(funName,ret,al,false) else call sigEntry,
	      if isspec then nil                       else call wrapperEntry )
	end
	  | processItem _ = ( nil , nil )

	(* STRUCTURE ENTRY GENERATION *)
	fun makeStructureEntry (FUNC(funName,ret,arglist)) =
	let
	    val al = splitArgTypes arglist
	    val wname = Util.computeWrapperName(space,funName)
	    fun line l io = 
		[wrIndent, "INIT_STRUCTURE(record, \"",nativeName,
		 "\", \"", wname, if io then "'" else "", "\", ", 
		 nativeName, "_", wname, if io then "_" else "", ", ",
		 Int.toString(numIns(l,io)), ", true);\n"]
	in
	    line al false @ (if numOuts(al,false)>0 then line al true else nil)
	end
	  | makeStructureEntry _ = nil

        (* STRUCTURE CODE GENERATION *)
        fun makeStructure items = 
	let
	    fun count arglist = 
		if numOuts(splitArgTypes(arglist),false) > 0 then 2 else 1
	    val numItems = 
		foldl (fn (FUNC(_,_,arglist),sum) => sum+(count arglist)
                                        | (_,sum) => sum) 0 items
	    fun getIncEntry filename =
	    let	val {base, ext} = OS.Path.splitBaseExt filename
		val ext' = case ext of SOME s => "_"^s | _ => ""
	    in	"_"^Util.strUpper(base)^Util.strUpper(ext')^"_"
	    end
	    val includeEntries = map getIncEntry Special.includeFiles

	    val header = 
		["word InitComponent() {\n",
		 "  includeData data[] = { ", 
		   (Util.makeTuple ", " "" includeEntries), " };\n",
		 "  Record *record = CreateRecord(data, ", 
		   Int.toString (length includeEntries), ", ",
		   Int.toString (numItems), ");\n\n"
		 ]
	    val footer = ["  RETURN_STRUCTURE(\"", nativeName,
			  "$\", record);\n}\n"]
	in
	    header@
	    (List.concat (map makeStructureEntry items))@
	    footer
	end

        (* main function for creating native files *)
        fun create tree =
	let
	    val myItems' = List.filter (Util.funNot Special.isIgnored) tree
	    val myItems = Util.filters [isItemOfSpace space, checkItem] 
			    (myItems'@Special.changedFuns@Special.specialFuns)

	    val _ = print ("Generating "^nativeName^"\n")
	    val s = Util.openFile siginfo
	    val w = Util.openFile wrapperinfo

	    val psl = Util.outputStrings
	    fun dpsl (sl, wl) = (psl s sl ; psl w wl )
        in
	    ( List.app (dpsl o processItem) myItems ;
	      psl w (makeStructure myItems) ;
	      Util.closeFile s ;
	      Util.closeFile w )
	end

    end
