(*
 * Authors:
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

structure Binding :> BINDING =
struct

    open TypeTree    
    open Util
    open Container
    open Parser


    (* Erstellt eine Liste y0,y1,y2,...,yn für einen Funktionsaufruf *)
    fun ParmList n nil = ""
      | ParmList n [VOID] = ""
      | ParmList n [t'] = cast t' ("y"^(Int.toString n))
      | ParmList n (t'::xr) = (cast t' ("y"^(Int.toString n))) ^ "," ^ (ParmList (n+1) xr)
  

    (* New Operator für eine Struktur *)
    fun create_struct_new struct_name =
	addFun(struct_name^"_new",[VOID],POINTER(STRUCTREF(struct_name)),
	       (cast (POINTER(STRUCTREF(struct_name))) 
		("malloc(sizeof("^(getCType(STRUCTREF(struct_name)))^"))")) ^";")
	

    (* Callbackfunktion für Struktureintrag *)

    fun createStructAccessor struct_name name ty =
	let val call_get = "("^(cast (POINTER(STRUCTREF struct_name)) "y0")^")"^"->"^name^";"
	    val call_set = "("^(cast (POINTER(STRUCTREF struct_name)) "y0")^")"^"->"^name^
		            " = " ^ (cast ty "y1") ^ ";"
	in addFun(struct_name^"_get_"^name,[POINTER(STRUCTREF struct_name)],ty,call_get);
	   addFun(struct_name^"_set_"^name,[POINTER(STRUCTREF struct_name),ty],VOID,call_set);()
	end

    fun BindingStructEntry struct_name (name,STRUCTREF n) = print ("Warning: Ignoring "^struct_name^"."^name^"\n")
      | BindingStructEntry struct_name (name,UNIONREF n) = print ("Warning: Ignoring "^struct_name^"."^name^"\n")
      | BindingStructEntry struct_name (name,ARRAY _) = print ("Warning: Ignoring Array "^struct_name^"."^name^"\n")
      | BindingStructEntry struct_name (name,TYPEREF(n,ty)) = BindingStructEntry struct_name (name,ty) 
      | BindingStructEntry struct_name (name,ENUMREF "") = createStructAccessor struct_name name (INT SIGNED)
      | BindingStructEntry struct_name (name,ENUMREF n) = createStructAccessor struct_name name (ENUMREF n)
      | BindingStructEntry struct_name (name,ty) = createStructAccessor struct_name name ty
   

    (* Erstellt das Binding *)
    fun create_binding x =
	let
	    
	    fun create_binding (FUNC(name,ret_ty, arg_ty)) = 
		
		(let val call = name ^ "(" ^ (ParmList 0 arg_ty) ^ ");"
		 in addFun(name,arg_ty,ret_ty,call); () end
		     handle (Warning s) => print ("Ignoring Function "^name^" Reason: "^s^"\n"))
		     
	      | create_binding (STRUCT("",entrys)) = print ("Warning: Ignoring structure with no name.\n")
		     
	      | create_binding (STRUCT(struct_name,entrys)) =
		     (if addType(struct_name,false,"") then
			  (addFun("cast_"^struct_name,[POINTER(TYPE_VAR NONE)],POINTER(STRUCTREF struct_name),
				  (cast (POINTER(STRUCTREF struct_name)) "y0") ^ ";");
			   addFun("sizeof_"^struct_name,[VOID],INT SIGNED,"sizeof("^struct_name^");");
			   create_struct_new struct_name;
			   map (BindingStructEntry struct_name) entrys; ())
		      else ()
			  handle (Warning s) => print ("Warning: "^s))
			  
	      | create_binding (UNION(struct_name,entrys)) =
			  create_binding (STRUCT(struct_name,entrys))

	      | create_binding (ENUM("",entrys)) =
			 ((map (addConst "int") entrys; ())
			  handle (Warning s) => print ("Warning: "^s))

	      | create_binding (ENUM(enum_name,entrys)) = 
			  (addEnum(enum_name,entrys);()
			  (*((addType(enum_name,false);
			   map (addConst enum_name) entrys; ()))*)
			   handle (Warning s) => print ("Warning: "^s))
			  
	      | create_binding _ = ()
			  
			  handle (Warning s) => print ("Warning: "^s)
	in
	    create_binding x
	       handle (Warning s) => print ("Warning: "^s)
	end

    fun register_simple_type ty =
	let val cty = getCType ty
	in
	    addFun("sizeof_"^cty,[VOID],INT SIGNED,"sizeof("^cty^");");
	    addFun("cast_"^cty,[POINTER(TYPE_VAR NONE)],POINTER ty,
		   (cast (POINTER ty) "y0") ^ ";");
	     addFun("unref_"^cty,[POINTER ty],ty,"*"^(cast (POINTER ty) "y0")^";");
	     addFun1("pointer_"^cty,[ty],POINTER ty,cty^"* r = "^(cast (POINTER ty) ("malloc(sizeof("^cty^"))"))^";\n\t*r = y0;")
	end

    (* Erstellt das Basisbinding *)

    fun createBasicBinding name  =
	let
	    val _ = print "Creating Binding ... "

	    val types_file = TextIO.openOut ("types.aml")
	    val _ = TextIO.output(types_file,
				  "structure types : \n"^
				  "sig\n\ttype 'a pointer\n\ttype cfun\nend\n=\n"^
				  "struct\n\ttype 'a pointer\n\ttype cfun\nend\n");
	    val _ = TextIO.closeOut types_file

	    val _ = Container.initialize name

	    (* Alle Basisfunktionen zum Container hinzufügen *)

	    
	    val _ = addType("pointer",true,"types")
	    val _ = addType("cfun",false,"types")

	    val _ = addFun("new",[INT SIGNED],POINTER(TYPE_VAR NONE),"malloc(y0);")
	    val _ = addFun("delete",[POINTER(TYPE_VAR NONE)],VOID,"free(y0);")
	    val _ = addFun1("pointer",[POINTER(TYPE_VAR(SOME 0))],POINTER(POINTER(TYPE_VAR(SOME 0))),
			    (getCType(POINTER(POINTER(TYPE_VAR(SOME 0)))))^" r = "^
			    (cast (POINTER(POINTER(TYPE_VAR(SOME 0)))) ("malloc(sizeof(void*))"))^";\n\t*r = y0;")
	    val _ = addFun("unref",[POINTER(POINTER(TYPE_VAR(SOME 0)))],POINTER(TYPE_VAR(SOME 0)),"*"^
			   (cast (POINTER(POINTER(TYPE_VAR(SOME 0)))) "y0")^";");
	    val _ = addFun("cast",[POINTER(TYPE_VAR(SOME 0))],POINTER(TYPE_VAR(SOME 1)),"y0;")
	   

	    val _ = register_simple_type (CHAR SIGNED)
	    val _ = register_simple_type (SHORT SIGNED)
	    val _ = register_simple_type (LONG SIGNED)
	    (*val _ = register_simple_type (LONGLONG SIGNED)*)
	    val _ = register_simple_type (INT SIGNED)
	    val _ = register_simple_type FLOAT
	    val _ = register_simple_type DOUBLE
	    (*val _ = register_simple_type LONGDOUBLE*)

	    val _ = print "(done)\n"
		
	    (* Binding speichern *)
	    val _ = print "Saving Binding ... "
	    val _ = saveBinding nil [("types","types")] nil
	    val _ = print "(done)\n"
	in () end

	
    (* Erstellt das Binding *)

    fun create header includes =
	let
	    val (name,ext) = Util.split_filename header
	    val _ = Container.initialize name

	    val _ = print ("Preparing file "^header^" ... ")
	    
	    val _ = Prepare.prepare header "cleaned.tmp"
	    val _ = print "(done)\n"
		
	    (* Headerfile parsen *)
	    val _ = print "Parsing ... "
	    val tree = Parser.parse "cleaned.tmp"
	    val _ = print "(done)\n"
		
	    val _ = print "Creating Binding ... "

	    val _ = addType("pointer",true,"C")
	    val _ = addType("cfun",false,"C")

	    val _ = map addType (Config.getTypeDep())

	    val _ = map create_binding tree
	    val _ = map Container.addUserFun (Config.getUserFuns())
	    val _ = print "(done)\n"

	    (* Binding speichern *)
	    val _ = print "Saving Binding ... "
	    val _ = saveBinding includes (("C","C")::(Config.getImports())) []
	    val _ = print "(done)\n"
	    
	in () end
end
