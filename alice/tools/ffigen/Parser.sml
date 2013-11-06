(*
 * Authors:
 *   Robert Grabowski <grabow@ps.uni-sb.de>
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

structure Parser :> PARSER =
struct
    open TypeTree
    open Util

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
	  | (w,0) => (print("WARNINGS: "^Int.toString(w));tree)
	  | (w,e) => (print ("ERRORS: "^Int.toString(e)^", "^
			     "WARNINGS: "^Int.toString(w));
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
	fun showWarning s = print ("Parser: "^s^"\n")
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

	    fun convType Ast.Void         = VOID
	      | convType Ast.Ellipses     = ELLIPSES true

	      | convType (Ast.Qual (Ast.CONST,t)) = CONST (convType t)
	      | convType (Ast.Qual (_,t)) = convType t

	      | convType (Ast.Numeric(_,_,Ast.UNSIGNED,Ast.CHAR,Ast.SIGNDECLARED)) = CHAR UNSIGNED
	      | convType (Ast.Numeric(_,_,_,Ast.CHAR,_)) = CHAR SIGNED
	     
	      | convType (Ast.Numeric(_,_,Ast.UNSIGNED,Ast.SHORT,Ast.SIGNDECLARED)) = SHORT UNSIGNED
	      | convType (Ast.Numeric(_,_,_,Ast.SHORT,_)) = SHORT SIGNED

	      | convType (Ast.Numeric(_,_,Ast.UNSIGNED,Ast.INT,Ast.SIGNDECLARED)) = INT UNSIGNED
	      | convType (Ast.Numeric(_,_,_,Ast.INT,_)) = INT SIGNED

	      | convType (Ast.Numeric(_,_,Ast.UNSIGNED,Ast.LONG,Ast.SIGNDECLARED)) = LONG UNSIGNED
	      | convType (Ast.Numeric(_,_,_,Ast.LONG,_)) = LONG SIGNED
	      
	      | convType (Ast.Numeric(_,_,Ast.UNSIGNED,Ast.LONGLONG,Ast.SIGNDECLARED)) = LONGLONG UNSIGNED
	      | convType (Ast.Numeric(_,_,_,Ast.LONGLONG,_)) = LONGLONG SIGNED

	      | convType (Ast.Numeric(_,_,_,Ast.FLOAT,_)) = FLOAT
	      | convType (Ast.Numeric(_,_,_,Ast.DOUBLE,_)) = DOUBLE
	      | convType (Ast.Numeric(_,_,_,Ast.LONGDOUBLE,_)) = LONGDOUBLE

	      | convType (Ast.Array(SOME (i,_),t)) = 
		          ARRAY (SOME (toSmallInt(i,"array size too big")), convType t)
	      | convType (Ast.Array (NONE, t))     = ARRAY (NONE, convType t)
	      | convType (Ast.Function (ret,arglist)) =
		          FUNCTION (convType ret, map convType arglist)
	      | convType (Ast.Pointer t) = POINTER (convType t)
	      | convType (Ast.StructRef id)  = STRUCTREF (getTypeNameFromID id)
	      | convType (Ast.UnionRef id)   = UNIONREF  (getTypeNameFromID id)
	      | convType (Ast.EnumRef id)    = ENUMREF   (findEnumName id)
	      | convType (Ast.TypeRef id)    = followType id
	      | convType _                   = raise EUnknown

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
	    fun parseMember (m:Ast.member,v) = (Symbol.name(#name m), LargeInt.toInt v)
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
	val astTree = generateAst file
	val result = parseDecls (filterCoreDecls (#ast astTree)) (#tidtab astTree)
    in
	result
    end

    (* Parsen eines Typs *)
    fun getType s nil = raise Error "getType: no type found!"
      | getType s ((FUNC(name,ret_ty, arg_ty))::xr) = 
	if s = name then ret_ty 
	else getType s xr
      | getType s (x::xr) = getType s xr

    fun parseType s = 
	let val file = TextIO.openOut "type.tmp"
	    val _ = TextIO.output(file,s^" f();")
	    val _ = TextIO.closeOut file
	    val tree = parse "type.tmp"	
	in  getType "f" tree  
	end

end
