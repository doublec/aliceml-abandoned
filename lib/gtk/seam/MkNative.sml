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
  This functor generates the native structure (.cc) and signature (.asig).
*)


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
		  "Modifications may get lost. *)\n",
		  "import structure Core from \"Core\"\n\n",
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
		  "#include \"Alice.hh\"\n",
		  "#include \"MyNativeAuthoring.hh\"\n",
		  "#include \"NativeUtils.hh\"\n",
		  (case Special.includeFile of
		      ("",_,_)   => "" 
		    | (name,_,_) => "#include \""^name^"\"\n"),
		  "\n\n"] ,
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
        fun wrapperEntry callLine (funName, ret, arglist, doinout) =
	let
	    (* Wrapper declaration line *)
	    val wrapperDecl = 
		["// ", getCFunType(funName,ret,arglist,false), 
		 "\nDEFINE", Int.toString(numIns (arglist,doinout)), 
		 "(", nativeName, "_", Util.computeWrapperName(space,funName), 
		 if doinout then "_" else "", ") {\n"]

            (* declaration of input or input/output arguments *)
	    fun inDeclLine (t,name,i) =
	    let
		val (macro, args) = fromWord t
	    in
		[wrIndent, macro, "(", Util.makeTuple 
		 ", " "" (name::("x"^(Int.toString i))::args), ");\n"]
	    end

	    (* declaration of output arguments *)
	    fun outDeclLine (t,name) = 
		    [wrIndent, getCType t, " ", name, outInit t, ";\n"]

	    (* all declarations *)
	    local
		val xCount = ref 0
		fun xCountInc () = ((!xCount) before xCount := (!xCount)+1)

		fun declare (IN, name, t) = inDeclLine (t,name,xCountInc())
		  | declare (OUT, name, t) =
		    if doinout 
			then inDeclLine(t,name,xCountInc()) 
		        else outDeclLine(t,name)
	    in
		val allDecls = map String.concat (map declare arglist)
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

		fun makeOutArg (_,name,t) =
		let
		    val (macro, args) = toWord t
		in
		    macro ^ "(" ^ (Util.makeTuple ", " "" (name::args)) ^ ")"
		end

		val retList = Util.makeTuple ", " "" (map makeOutArg outs)
		val retList' = if retList = "" then "" else ("("^retList^")")
	    in
		val retLine = 
		    [wrIndent, "RETURN", Int.toString (length outs),
		               retList', ";\n"]
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
		 wrIndent, "Record *record = Record::New(", 
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
