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
 The Parser structure creates a representation of the declarations
 in a C file in two steps:
     1. The CKIT is used to generate an abstract syntax tree (AST) 
        from the C file. 
     2. The functions in this structure then convert the CKIT AST
        into an own simplified representation (defined in TypeTree.sml).
*)


structure Parser :> PARSER =
struct
    open TypeTree

    (* EParseError: thrown if error during CKIT parsing occurs *)
    exception EParseError

    (* EMessage: thrown when a declaration cannot be converted *)
    exception EMessage of string 

    (* EIgnore: thrown when a part of a declaration (like an enum value) *)
    (*          cannot be converted *)
    exception EIgnore of string      

    (* EUnknown: general purpose exception *)
    exception EUnknown           



    (* Use the CKIT to generate an AST from a C file *)
    fun generateAst file =
    let
	val tree = ParseToAst.fileToAst file
    in
	case (#warningCount tree, #errorCount tree) of
	    (0,0) => tree
	  | (w,0) => (print(Util.separator("WARNINGS: "^Int.toString(w)));tree)
	  | (w,e) => (print (Util.separator("ERRORS: "^Int.toString(e)^", "^
					    "WARNINGS: "^Int.toString(w))) ;
			 raise EParseError )
    end
	
    (* Remove declarations from AST that are not relevant for the binding *)
    fun filterCoreDecls decls = 
	foldr (fn (Ast.DECL(Ast.ExternalDecl d,_,_),e) => d::e 
                                               | (_,e) => e) 
	nil 
	decls

    (* Main AST->TypeTree conversion function. *)
    fun parseDecls decls ttab =
    let
	(* Utility functions *)
	fun showWarning s = () (*print ("Parser: "^s^"\n")*) (*only for debug*)
        fun toSmallInt (i,s) = (LargeInt.toInt i) handle _ => raise EIgnore s
	fun parseList f memlist =
	    foldr (fn (x,l) => ((f x)::l) handle EIgnore s=>(showWarning s; l))
	          nil memlist

        (* Type ID table functions *)
	fun getTypeByID id = 
	    (valOf(Tidtab.find(ttab, id)):Bindings.tidBinding)
	    handle 
	        _ => raise EMessage ("TypeID not found: "^(Int.toString id))

	fun getTypeName ({name,...} : Bindings.tidBinding) =
	    case name of
		SOME s => s 
	      | _      => ""

	fun getTypeNameFromID id = getTypeName (getTypeByID id)

        fun findEnumName enumid =
        let
	    fun find (Ast.TypeDecl({tid,...})) =
		(case (#ntype (getTypeByID tid)) of
		    SOME (Bindings.Typedef(_,Ast.EnumRef id)) => 
			if id = enumid then getTypeNameFromID tid else ""
		  | _ => "")
	      | find _ = ""
	in
	    foldl (fn (d,e) => if find d = "" then e else find d) "" decls
	end
	    
	(* Ast.ctype to TypeTree.ty conversion *)
	local
	    fun convNumeric Ast.CHAR = CHAR
	      | convNumeric Ast.SHORT = SHORT
	      | convNumeric Ast.INT = INT
	      | convNumeric Ast.LONG = LONG
	      | convNumeric Ast.LONGLONG = LONGLONG
	      | convNumeric Ast.FLOAT = FLOAT
	      | convNumeric Ast.DOUBLE = DOUBLE
	      | convNumeric Ast.LONGDOUBLE = LONGDOUBLE
	    fun isReal k = k=FLOAT orelse k=DOUBLE orelse k=LONGDOUBLE

	    fun isBool "gboolean" = true
	      | isBool _          = false

	    fun isValist "va_list" = true
	      | isValist _         = false

            fun makePointer (TYPEREF("GList",_))  = LIST("GList",POINTER VOID)
	      | makePointer (TYPEREF("GSList",_)) = LIST("GSList",POINTER VOID)
	      | makePointer (NUMERIC (sg,_,CHAR)) = STRING sg
	      | makePointer (tr as TYPEREF(_,t))  =
		   (case makePointer t of LIST l   => LIST l
			                | ELLIPSES t => ELLIPSES t
		                        | STRING s => STRING s
                                        | _        => POINTER tr)
	      | makePointer t                      = POINTER t
                                
	in
	    fun convType Ast.Void         = VOID
	      | convType Ast.Ellipses     = ELLIPSES true
	      | convType (Ast.Qual (_,t)) = convType t
	      | convType (Ast.Numeric(_,_,sign,kind,tag)) = 
  	        NUMERIC (not (sign=Ast.UNSIGNED andalso tag=Ast.SIGNDECLARED),
			 isReal (convNumeric kind), convNumeric kind)
	      | convType (Ast.Array(SOME (i,_),t)) = 
		ARRAY (SOME (toSmallInt(i,"array size too big")), convType t)
	      | convType (Ast.Array (NONE, t))     = ARRAY (NONE, convType t)
	      | convType (Ast.Pointer (Ast.Function (ret,arglist))) =
		FUNCTION (convType ret, map convType arglist)
	      | convType (Ast.Pointer t) = makePointer (convType t)
	      | convType (Ast.Function (ret,arglist)) = raise EUnknown
	      | convType (Ast.StructRef id)  = STRUCTREF (getTypeNameFromID id)
	      | convType (Ast.UnionRef id)   = UNIONREF  (getTypeNameFromID id)
	      | convType (Ast.EnumRef id)    = ENUMREF   (findEnumName id)
	      | convType (Ast.TypeRef id)    = 
                  if isBool (getTypeNameFromID id) then BOOL else 
		  if isValist (getTypeNameFromID id) then ELLIPSES false else
		      followType id
	      | convType Error               = raise EUnknown

	    and followType id = 
		let
		    val t = 
			case getTypeByID id of 
			    {ntype=SOME(Bindings.Typedef(_,t')),...} => 
				convType t'
			  | _ => raise EUnknown
		in
		    TYPEREF (getTypeNameFromID id, t)
		end
	end

        (* The following functions parse struct, union, enum, *)
        (* typedef and function declarations. *)
	fun parseStruct structName memlist = 
	let	    
	    val message = "ignored anonymus bitfield in struct "^structName
	    fun parseMember (_, NONE, _) = raise EIgnore message
	      | parseMember (t, SOME (m:Ast.member), _) =
		(Symbol.name(#name m), convType t)  (* bitfield size ignored *)
	in
	    STRUCT (structName, parseList parseMember memlist)
	end

	fun parseUnion unionName  memlist = 
        let
	    fun parseMember(t,m:Ast.member) = (Symbol.name(#name m),convType t)
	in
	    UNION (unionName, parseList parseMember memlist)
	end

	fun parseEnum tid memlist =
	let
	    fun parseMember (m:Ast.member,v) = (Symbol.name(#name m), v)
	in
	    ENUM (findEnumName tid, map parseMember memlist)
	end

	fun parseTypeDef typeName ctype = ALIAS (typeName, convType ctype)

	fun parseFun ({name,ctype,...}:Ast.id) = 
	let
	    val symid = Symbol.name (name)
	    fun error s = raise EMessage("ignored function "^symid^": "^s)
	in
	    case ctype of
		Ast.Function (ret,arglist) => 
		( 
		    FUNC (symid, convType ret, map convType arglist)
		handle
		    EUnknown  => error "unknown type in arglist or retval!"
	        )
	      | _ => 
		raise EMessage("ignored "^symid^": probably external variable")
	end

   
	fun parseDecls' nil     parsed unparsed = rev parsed
	  | parseDecls' (d::ds) parsed unparsed =
	    (case d of
		 Ast.VarDecl(id,_)       => 
		     parseDecls' ds ((parseFun id)::parsed) unparsed
	       | Ast.TypeDecl({tid,...}) => 
		 let 
		     val t = getTypeByID tid
		     val name = getTypeName t
		     val p =   
			 (case (#ntype t) of
			      SOME (Bindings.Typedef(tid,ctype))  => 
				  parseTypeDef name ctype
			    | SOME (Bindings.Enum(tid,memlist))   => 
				  parseEnum tid memlist
			    | SOME (Bindings.Struct(_,memlist)) => 
				  parseStruct name memlist
			    | SOME (Bindings.Union(_,memlist))  => 
				  parseUnion name memlist
			    | _ => raise EMessage("ignored partial type "^name)
			 )
		 in
		     parseDecls' ds (p::parsed) unparsed
		 end
	    )
	    handle 
	        EMessage s => ( showWarning s ; 
			        parseDecls' ds parsed (d::unparsed) )
	      | EIgnore s  => ( showWarning s ;
			        parseDecls' ds parsed (d::unparsed) )
	      | _          => parseDecls' ds parsed (d::unparsed)
    in
	parseDecls' decls nil nil
    end (* of parseDecls *)

    (* Main parse function *)
    fun parse file =
    let
	val _ = print (Util.separator "Generating AST")
	val astTree = generateAst file
	val _ = print (Util.separator "Generating own type tree")
	val result = parseDecls (filterCoreDecls (#ast astTree)) 
                                (#tidtab astTree)
    in
	result
    end
end
