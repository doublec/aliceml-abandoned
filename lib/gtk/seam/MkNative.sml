
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
		  Util.indent 1, "sig\n"] ,
	     outro = 
		 [Util.indent 1, "end\n",
		  "end\n\n"]
            } : Util.fileInfo

	val wrapperinfo = {
             name = nativeName^".cc" ,
	     intro = 
		 ["// This is a generated file. ",
		  "Modifications may get lost.\n\n",
		  "#include \"NativeUtils.hh\"\n",
		  (case Special.includeFile of
		      ("",_,_)   => "" 
		    | (name,_,_) => "#include \""^name^"\"\n"),
		  "\n\n"] ,
             outro = []
            } : Util.fileInfo

        local
	    val classes = ref nil
	    exception NoUnref

	    fun buildClassList' (STRUCT (name,(_,t)::_)) =
		(case removeTypeRefs t of
		     STRUCTREF sup => ( classes := ((sup,name)::(!classes)) )
		   | _             => () 
		)
	      | buildClassList' _ = ()

	    fun getParentClass name nil = raise NoUnref
	      | getParentClass name ((sup, n)::cs) = 
		if n=name then sup else getParentClass name cs

	    fun getUnrefFun' "_GObject"   = "TYPE_G_OBJECT"
	      | getUnrefFun' "_GtkObject" = "TYPE_GTK_OBJECT"
	      | getUnrefFun' name        = 
		      getUnrefFun' (getParentClass name (!classes))
	in
	    fun buildClassList tree = List.app buildClassList' tree
	    fun getTypeInfo t = 
		(case removeTypeRefs t of 
		     STRUCTREF name => getUnrefFun' name
		   | _              => raise NoUnref)
		     handle _ => "TYPE_UNKNOWN"
	end
	    
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
        fun wrapperEntry callLine (funName, ret, arglist, doinout) =
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
		  | inDeclare (ELLIPSES true)     = "DECLARE_ELLIPSES"
		  | inDeclare (ELLIPSES false)    = "DECLARE_VALIST"
		  | inDeclare BOOL                = "DECLARE_BOOL"
		  | inDeclare (POINTER _)         = "DECLARE_OBJECT"
		  | inDeclare (STRING _)          = "DECLARE_CSTRING"
		  | inDeclare (FUNCTION _)        = "DECLARE_OBJECT"
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
			[wrIndent,inDeclare(t),"(",name,",x",xcinc(),");\n"]
	    end

	    (* declaration of output arguments *)
	    local
		fun outInit (NUMERIC _)      = " = 4711"
		  | outInit BOOL             = " = true"
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
		  | cArgList' (IN,name,(ELLIPSES true)) = 
                    let
			val nums = List.tabulate(2,
				      (fn i => name^"["^Int.toString(i)^"]"))
		    in
			Util.makeTuple ", " "" nums
		    end
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
				     | ELLIPSES _ => reintCast
				     | _          => staticCast
		    in
			cast (getCType t) name
		    end
		val cArgList = Util.makeTuple ", " "" (map cArgList' arglist)
	    in
		val funCall = if null callLine then
		              [wrIndent,
			       (case ret of
				   VOID => ""
				 | STRING _ =>"const "^(getCType ret)^" ret = "
				 | _ => (getCType ret)^" ret = "),
				funName, "(", cArgList, ");\n"]
			      else
			      [wrIndent] @ callLine
	    end

	    (* return line *)
	    local
	        val alwithret = 
		    if ret = VOID then arglist else (OUT, "ret", ret)::arglist
		val (_,outs) = splitInOuts (alwithret, doinout)

		fun retConv (NUMERIC(_,false,_)) n = "INT_TO_WORD("^n^")"
		  | retConv (NUMERIC(_,true ,_)) n = "REAL_TO_WORD("^n^")"
		  | retConv BOOL                 n = "BOOL_TO_WORD("^n^")"
		  | retConv (POINTER p)          n =
		         "PointerToObject("^n^","^(getTypeInfo p)^")"
                  | retConv (STRING _)           n ="STRING_TO_WORD("^n^")"
		  | retConv(LIST(tname,STRING _))n =tname^"ToStringList("^n^")"
		  | retConv(LIST(tname,_))       n =tname^"ToObjectList("^n^")"
		  | retConv (FUNCTION _)         n =
			 "PointerToObject((void*)("^n^"),0)"
		  | retConv (TYPEREF(_,t))       n = retConv t n
		  | retConv (ENUMREF _)          n = "ENUM_TO_WORD("^n^")"
		  | retConv _                    n = n
  
		fun makeOutArg (_,name,t) = retConv t name
		val retList = Util.makeTuple ", " "" (map makeOutArg outs)
	    in
		val retLine = 
		    [wrIndent, "RETURN_TUPLE",Int.toString (length outs),
		               "(",retList,");\n"]
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

        (* special preparation for get/set methods *)
        fun fieldSigEntry (sname, mname, mtype, get) =
	let
	    val (funName,ret,arglist) = makeFieldFun space 
		                          (sname,mname,mtype,get)
	in
            sigEntry (funName,ret,splitArgTypesNoOuts arglist,false)
	end

        fun fieldWrapperEntry (sname, mname, mtype, get) = 
        let
	    val (funName,ret,arglist) = makeFieldFun space 
		                          (sname,mname,mtype,get)
	    val al = splitArgTypesNoOuts arglist
	    val stype = if null al then "" else getCType (#3(hd al))
	    val svar  = if null al then "" else #2(hd al)
	    val mvar  = if length al < 2 then "" else #2(hd(tl al))
	    val isString = case mtype of STRING _ => true | _ => false
	    val callLine =
		if get then
		    [if isString then "const " else "",
		     getCType mtype, " ret = (static_cast<",
		     stype, ">(", svar,"))->", mname,";\n"]
		else
		    ["(static_cast<", stype, ">(", svar,"))->", mname, " = ",
		     if isString then "reinterpret_cast<" else "static_cast<",
		     getCType mtype, ">(", mvar, ");\n"]
	in
	    wrapperEntry callLine (funName,ret,al,false)
	end

        (* SIGNATURE AND WRAPPER ENTRIES *)
	fun processItem (func as FUNC (funName,ret,arglist)) =
	    let
		val al = splitArgTypes arglist
		fun call f = f(funName,ret,al,false) @
		                 (if numOuts(al,true) > 0
				      then f(funName,ret,al,true) 
				      else nil)
		val spec = Util.contains func Special.specialFuns
	    in
	       (if spec then sigEntry(funName,ret,al,false) else call sigEntry,
	        if spec then nil                 else call (wrapperEntry nil) )
	    end
	  | processItem (STRUCT (structName, members)) =
	    let
	        fun call f get = 
		    List.concat 
		      (map (fn (mname, mtype) => f(structName,mname,mtype,get))
		           (List.filter checkStructMember members))
	    in
	        ( List.concat (map (call fieldSigEntry) [true,false]),
		  List.concat (map (call fieldWrapperEntry) [true,false]) )
	    end
	  | processItem (UNION (unionName, members)) =
	      processItem (STRUCT (unionName, members))
	  | processItem _ = ( nil , nil )


	(* STRUCTURE ENTRY GENERATION *)
	local
	    fun line (funName,arglist,io) =
	    let
		val wname = Util.computeWrapperName(space,funName)
	    in
		String.concat
		    [wrIndent, "INIT_STRUCTURE(record, \"",nativeName,
		     "\", \"", wname, if io then "'" else "", "\", ", 
		     nativeName, "_", wname, if io then "_" else "", ", ",
		     Int.toString(numIns(arglist,io)), ");\n"]
	    end
	in
	    fun makeStructureEntry (FUNC(funName,ret,arglist)) =
		let
		    val al = splitArgTypes arglist
		in
		    line(funName,al,false) ::
		      (if numOuts(al,false)>0 
			   then [line(funName,al,true)] else nil)
		end
	      | makeStructureEntry (STRUCT(structName, members)) =
		let
		    fun fieldGetSetInit sname get (mname,mtype) =
		    let
			val (funName,ret,arglist) = 
			    makeFieldFun space (sname,mname,mtype,get)
		    in
			line(funName,splitArgTypesNoOuts arglist,false)
		    end
		    val members' = List.filter checkStructMember members
		in
		    (map (fieldGetSetInit structName true)  members') @
		    (map (fieldGetSetInit structName false) members')
		end
	      | makeStructureEntry (UNION(unionName, members)) =
		    makeStructureEntry (STRUCT(unionName, members))
	      | makeStructureEntry _ = nil
	end

        (* STRUCTURE CODE GENERATION *)
        fun makeStructure items = 
	let
	    val initLines = List.concat (map makeStructureEntry items)

	    val (_, initFun, addEntries) = Special.includeFile
	    val header = 
		["word InitComponent() {\n",
		 wrIndent, "Record *record = CreateRecord(", 
		   Int.toString ((length initLines)+addEntries), ");\n"] @
		(if initFun = "" 
		     then nil 
		     else [wrIndent, initFun, "(record);\n"])@
                ["\n"]
	    val footer = [wrIndent, "RETURN_STRUCTURE(\"", nativeName,
			  "$\", record);\n}\n"]
	in
	    header @ initLines @ footer
	end

        (* main function for creating native files *)
        fun create tree =
	let
	    val _ = print (Util.separator("Generating "^nativeName))
	    val _ = buildClassList tree
	    val myItems' = List.filter (Util.funNot Special.isIgnored) tree
	    val myItems = Util.filters [isItemOfSpace space, checkItem] 
			    (myItems'@Special.changedFuns@Special.specialFuns)

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
