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


structure Container :> CONTAINER =
struct

    open TypeTree
    open Util
    open Config

    (* Bereits berechnete Zeilen in Signatur,
       Binding und InitComponent Datei *)

    val sign = ref nil : string list ref
    val binding = ref nil : string list ref
    val component = ref nil : string list ref

    val sign_new = ref NONE : string option ref
    val binding_new = ref NONE : string option ref
    val component_new = ref NONE : string option ref

    val fun_set = ref (Set.emptyset String.compare)
    val type_set = ref (Set.emptyset String.compare)

    (* Zugriffsfunktionen auf obige Listen *)

    fun add_sign_string s = sign_new := SOME s
    fun add_binding_string s = binding_new := SOME s
    fun add_component_string s = component_new := SOME s

    fun update_obj obj obj_new = 
	case !obj_new of 
	    NONE => () 
	  | (SOME s) => (obj := s::(!obj);
			 obj_new := NONE)

    fun update() = (update_obj sign sign_new;
		    update_obj binding binding_new;
		    update_obj component component_new)


    (* Name des Bindings *)

    val my_name = ref "noname" 
    fun setName name = my_name := name
    fun getName() = !my_name


    (* Helper Functions *)

    fun CreateProduct 0 = ""
      | CreateProduct n = "*"^(CreateProduct(n-1))
	
    fun HandlePtr n (POINTER(FUNCTION x)) = SOME(n+1,FUNCTION x)
      | HandlePtr n (POINTER(ARRAY x)) = SOME(n+1,ARRAY x)
      | HandlePtr n (POINTER t) = HandlePtr (n+1) t
      | HandlePtr n _ = NONE

    fun initialize name = setName name
	 
  (* Convert a TypeTree type into its C type name *)    

    local
	fun space "" = ""
	  | space s = " " ^ s

	fun getCType' obj (CHAR SIGNED)  = "char" ^ (space obj)
	  | getCType' obj (CHAR UNSIGNED) = "unsigned char" ^ " " ^ (space obj) 
	  | getCType' obj (SHORT SIGNED) = "short" ^ (space obj)
	  | getCType' obj (SHORT UNSIGNED) = "unsigned short" ^ (space obj)
	  | getCType' obj (INT SIGNED) = "int" ^ (space obj)
	  | getCType' obj (INT UNSIGNED) = "unsigned int" ^ (space obj)
	  | getCType' obj (LONG SIGNED) = "long" ^ (space obj)
	  | getCType' obj (LONG UNSIGNED) = "unsigned long" ^ (space obj)
	  | getCType' obj (LONGLONG SIGNED) = "long long" ^ (space obj)
	  | getCType' obj (LONGLONG UNSIGNED) = "unsigned long long" ^ (space obj)
	  | getCType' obj FLOAT = "float" ^ (space obj)
	  | getCType' obj DOUBLE = "double" ^ (space obj) 
	  | getCType' obj LONGDOUBLE = "long double" ^ (space obj)
	  | getCType' obj VOID = "void" ^ (space obj)
	  | getCType' obj (CONST t) = "const " ^ (getCType'' obj t)
	  | getCType' obj (ELLIPSES _) = raise Warning "Ellipses not supported."
	  | getCType' obj BOOL = "bool" ^ (space obj)
	  | getCType' obj STRING = "char*" ^ (space obj)
	  | getCType' obj (POINTER(ARRAY c)) = "("^(getCType'' obj (ARRAY c))^")*"
	  | getCType' obj (POINTER t) = (getCType'' "" t)^"*"^(space obj)
	  | getCType' obj (ARRAY(NONE,t)) = (getCType'' "" t)^ (space obj)^ " []"
	  | getCType' obj (ARRAY(SOME n,t)) = (getCType'' "" t) ^(space obj) ^"["^(Int.toString n)^"]"
	  | getCType' obj (LIST t) = raise Warning "Lists not supported.." (*"list<"^(getCType'' obj t)^">" ^ " " ^ obj *)
	  | getCType' obj (FUNCTION(t,tr)) = "cfun" (*raise Error "Internal Error, getCType' FUNCTION"*)
	  | getCType' obj (STRUCTREF s) = s ^ (space obj)
	  | getCType' obj (UNIONREF s) = s ^ (space obj)
	  | getCType' obj (ENUMREF s) = s ^ (space obj)
	  | getCType' obj (TYPEREF(_,ENUMREF s)) = s ^ (space obj)
	  | getCType' obj (TYPEREF(s,t)) = (getCType'' obj t; s ^ (space obj))
	  | getCType' obj (TYPE_VAR _) = "void" ^(space obj)
	    
	and getCType'' obj t = 
	    case HandlePtr 0 t of
		(SOME(n,FUNCTION(t,tr))) => 
	                        (getCType'' "" t)^"(" ^ (CreateProduct n)^obj^")("^(Kommas(map (getCType'' "") tr)) ^ ")"
	      |(SOME(n,ARRAY(NONE,ty))) => (getCType'' "" ty)^"("^ (CreateProduct n)^ obj ^ ")[]" 
	      |(SOME(n,ARRAY(SOME m,ty))) => (getCType'' "" ty)^"("^(CreateProduct n)^ obj^ ")["^(Int.toString m)^"]"
	      | NONE => getCType' obj t
	      | _ => raise Error "Internal Error: getCType"
    in
	fun getCType t = getCType'' "" t
	fun getCDecl(name,t) = getCType'' name t
    end

    (* Convert a TypeTree type into its alice type name *)

    fun getAliceType ty =
	let
	    val c = ref 0

	    fun getTyVar n = "'"^(String.str(Char.chr((n)+Char.ord(#"a"))))

	    fun handle_type_name s = 
		(case Config.applyFilter s of
		     CMD_REJECT => raise Warning ("Filter: type "^s^" ignored.")
		   |(CMD_ACCEPT s') => (if not (Set.member(!type_set,s'))
				       then (sign := ("\t\ttype " ^ s')::(!sign); type_set := Set.insert(!type_set,s'))
				       else (); 
				       s'))
		
	    fun getAliceType' (CHAR _) = "int"
	      | getAliceType' (SHORT _) = "int"
	      | getAliceType' (INT _) = "int"
	      | getAliceType' (LONG _) = "int"
	      | getAliceType' (LONGLONG _) = "int"
	      | getAliceType' FLOAT = "real"
	      | getAliceType' DOUBLE = "real"
	      | getAliceType' LONGDOUBLE = "real"
	      | getAliceType' VOID = "unit"
	      | getAliceType' (CONST t) = getAliceType' t
	      | getAliceType' (ELLIPSES _) = raise Warning "Ellipses not supported."
	      | getAliceType' BOOL = "boolean"
	      | getAliceType' (POINTER(FUNCTION(t,tr))) = "cfun"
	      | getAliceType' (POINTER VOID) = getAliceType'' (POINTER(TYPE_VAR NONE))
	      | getAliceType' (POINTER(CONST(VOID))) = getAliceType'' (POINTER(TYPE_VAR NONE))
	      | getAliceType' (POINTER t) = getAliceType'' t^" pointer"
	      | getAliceType' STRING = "string"
	      | getAliceType' (ARRAY(NONE,t)) = (getAliceType'' t)^" Array.array"
	      | getAliceType' (ARRAY(SOME n,t)) = (getAliceType'' t)^" Array.array"
	      | getAliceType' (LIST t) = (getAliceType'' t)^" list"
	      | getAliceType' (FUNCTION(t,tr)) = (Product(map getAliceType'' tr))^" -> "^(getAliceType'' t)
	      | getAliceType' (STRUCTREF s) = handle_type_name s
	      | getAliceType' (UNIONREF s) = handle_type_name s
	      | getAliceType' (ENUMREF s) = handle_type_name s
	      | getAliceType' (TYPEREF(_,ENUMREF s)) = handle_type_name s
	      | getAliceType' (TYPEREF(_,t as (FUNCTION _))) = "cfun"
	      | getAliceType' (TYPEREF(s,t)) = handle_type_name s
	      | getAliceType' (TYPE_VAR NONE) =  getTyVar (!c) before c := !c + 1
	      | getAliceType' (TYPE_VAR(SOME n)) = getTyVar n
		
	    and getAliceType'' ty = 
		case Config.isSpecialType(getCType ty) of
		    NONE => getAliceType' ty
		  | (SOME{alicetype=alicetype',...}) => alicetype'
	in
	  getAliceType'' ty  
	end

    (* Wandelt ein Word in den C-Wert um *)

    fun fromWord' (CHAR _) = "DECLARE_CHAR"
      | fromWord' (SHORT _) = "DECLARE_SHORT"
      | fromWord' (INT _) = "DECLARE_INT"
      | fromWord' (LONG _) = "DECLARE_LONG"
      | fromWord' (LONGLONG _) = "DECLARE_LONGLONG"
      | fromWord' (FLOAT) = "DECLARE_CFLOAT"
      | fromWord' (DOUBLE) = "DECLARE_CDOUBLE"
      | fromWord' (LONGDOUBLE) = "DECLARE_CLONGDOUBLE"
      | fromWord' (VOID) = raise Warning "fromWord: VOID"
      | fromWord' (CONST t) = fromWord' t
      | fromWord' (ELLIPSES _) = raise Warning "Ellipses not supported."
      | fromWord' (BOOL) = "DECLARE_BOOL"
      | fromWord' (POINTER ty) = "DECLARE_UNMANAGED_POINTER"
      | fromWord' (STRING) = "DECLARE_CSTRING"
      | fromWord' (ARRAY(NONE,_)) = raise Warning "fromWord' Array"
      | fromWord' (ARRAY(SOME i,ty)) = "DECLARE_CARRAY"
      | fromWord' (LIST _) = "DECLARE_LIST"         
      | fromWord' (FUNCTION _) = raise Warning "fromWord' Function"
      | fromWord' (STRUCTREF s0) = raise Warning ("fromWord' "^s0)
      | fromWord' (UNIONREF s0) = raise Warning ("fromWord' "^s0)
      | fromWord' (ENUMREF s0) = "DECLARE_ENUM"
      | fromWord' (TYPEREF(s0,ty)) = fromWord ty
      | fromWord' (TYPE_VAR _) = raise Warning "fromWord: TYPE_VAR"

    and fromWord ty =
	case  Config.isSpecialType(getCType ty) of
	    NONE => fromWord' ty
	  | (SOME{fromword=fromword',...}) => fromword'


    (* Wandelt einen C-Wert in ein Word um *)

    fun toWord' (CHAR _) = "CHAR_TO_WORD"
      | toWord' (SHORT _) = "SHORT_TO_WORD"
      | toWord' (INT _) = "INT_TO_WORD"
      | toWord' (LONG _) = "LONG_TO_WORD"
      | toWord' (LONGLONG _) = "LONGLONG_TO_WORD"
      | toWord' FLOAT = "FLOAT_TO_WORD"
      | toWord' DOUBLE = "DOUBLE_TO_WORD"
      | toWord' LONGDOUBLE = "LONGDOUBLE_TO_WORD"
      | toWord' VOID = raise Warning "toWord: VOID"
      | toWord' (CONST t) = toWord' t
      | toWord' (ELLIPSES _) = raise Warning "Ellipses not supported."
      | toWord' BOOL = "BOOL_TO_WORD"
      | toWord'(POINTER t) = "UNMANAGED_POINTER_TO_WORD"
      | toWord' STRING = "STRING_TO_WORD"
      | toWord' (ARRAY _) = raise Warning "Array to Word not supported."
      | toWord' (LIST _) = "LIST_TO_WORD"         
      | toWord' (FUNCTION _) = raise Warning "toWord' Function"
      | toWord' (STRUCTREF s) = raise Warning ("toWord' "^s)
      | toWord' (UNIONREF s) = raise Warning ("toWord' "^s)
      | toWord' (ENUMREF s) = "ENUM_TO_WORD"
      | toWord' (TYPEREF(s,t)) = toWord t
      | toWord' (TYPE_VAR _) = raise Warning "toWord: TYPE_VAR"
	
    and toWord ty =
	case  Config.isSpecialType(getCType ty) of
	    NONE => toWord' ty
	  | (SOME{toword=toword',...}) => toword'

		
    (* Führt einen Cast durch *)

    fun cast (ARRAY _) v = v
      | cast ty v = "("^(getCType ty)^")"^v


    (* Wandelt ein Word in einen Typ um *)

    fun MyFromWord (t,s,ARRAY(NONE,_)) = raise Warning "MyFromWord Array"
      | MyFromWord (t,s,ty' as (ARRAY(SOME i,ty))) = (fromWord ty') ^"("^t^","^s^","
	                                             ^(getCType ty)^","^(fromWord ty)^")"
      | MyFromWord (t,s,ty' as (ENUMREF s0)) = (fromWord ty') ^"("^t^","^s^","^(getCType ty')^")"
      | MyFromWord (t,s,FUNCTION _) = raise Warning "MyFromWord: FUNCTION"
      | MyFromWord (t,s,STRUCTREF s0) = raise Warning "MyFromWord: STRUCTREF"
      | MyFromWord (t,s,UNIONREF s0) = raise Warning "MyFromWord UNIONREF"
      | MyFromWord (t,s,TYPEREF(s0,ty)) = MyFromWord(t,s,ty)
      | MyFromWord (t,s,ty) = (fromWord ty) ^"("^t^","^s^")"

    (* Erstellt falls nötig einen const_cast *)

    fun PtrCast (POINTER(CONST t)) v = "const_cast<"^(getCType(POINTER(t)))^">("^v^")"
      | PtrCast (CONST t) v = "const_cast<"^(getCType t)^">("^v^")"
      | PtrCast _ v = v

    (* Wandelt einen Typ in ein Word um *)

    fun MyToWord (s,ty' as (ENUMREF s0)) = (toWord ty') ^ "(" ^ s ^ "," ^ (getCType ty') ^ ")" 
      | MyToWord (s,TYPEREF(s0,ty)) = MyToWord(s,ty)
      | MyToWord (s,ty as (POINTER t)) = let val toword = toWord ty
					 in case toword of 
					     "UNMANAGED_POINTER_TO_WORD" => (toWord ty) ^ "("^"(void*)("^(PtrCast ty s) ^ ")"  ^ ")"
					   | _ => (toWord ty) ^ "("^(PtrCast ty s) ^ ")" 
					 end
      | MyToWord (s,ty) = (toWord ty) ^ "("^(PtrCast ty s) ^ ")" 
	
    (* Wandelt alle Parameter von Words in C-Werte um *)

    fun DeclareParameters n nil = ""
      | DeclareParameters n (VOID::xr) = DeclareParameters (n+1) xr
      | DeclareParameters n (x::xr) = "\t"^MyFromWord("y"^(Int.toString n),"x"^(Int.toString n),x)^";\n" ^
	DeclareParameters (n+1) xr  

	
    (* Erstellt einen Funktionsaufruf *)

    fun createFunCall(name,ret_ty,arg_ty,call,r) =
	("DEFINE" ^ (Int.toString (length arg_ty)) ^ "(" ^ name ^") {\n" ^
	(DeclareParameters 0 arg_ty) ^
	(case ret_ty of 
	     VOID => "\t"^call ^ "\n" ^ "\tRETURN_UNIT;\n"
	   | _    => "\t"^(if r then (getCDecl("r",ret_ty)) ^ " = " else "") ^ call ^ "\n" ^
		 "\t"^"word rw = " ^ MyToWord("r",ret_ty) ^ ";\n" ^ 
		 "\tRETURN(rw);\n")^
	     "} END \n\n" )

 	
    (* Zugriffsfunktionen auf obige Listen *)

    fun add_sign(name,ty) = 
	add_sign_string("\t\tval "^name^" : "^(getAliceType ty))

    fun add_binding_fun(name,ret_ty,arg_ty,body,r) =
	add_binding_string(createFunCall(name^"_intern",ret_ty,arg_ty,body,r))

    fun add_component_fun(name,nargs) =
	add_component_string("\tINIT_STRUCTURE(record,\""^(getName())^"\",\""^name^"\", "^
			     name^"_intern,"^(Int.toString nargs)^ ");")

    fun add_component_const(name,value) =
	add_component_string("\tINIT_STRUCTURE_INT(record,\""^name^"\","^(Int.toString value)^");" )


    (* Fügt eine Funktion zum Container hinzu *)

    fun addUserFun { name = name', typ = typ', export = export', body = body' } = 
	(print("Adding user defined function "^name'^"\n");
	 add_sign_string ("\t\tval "^name'^" : "^typ'^"\n");
	 add_binding_string body';
	 add_component_string ("\t"^export');
	 update())

    fun addFun_intern(name,param,result,body) b =
	if length param > 16 then raise Warning "Functions with more than 16 Arguments are not supported."
	else
	    (add_sign(name,FUNCTION(result,param));
	     add_binding_fun(name,result,param,body,b);
	     add_component_fun(name,length param);
	     update(); true)

    fun addFun'(x as (name,param,result,body)) b =
	(case Config.applyFilter name of
	     CMD_REJECT => ((*print ("Filter: "^ name ^ " rejected.\n");*) false)
	   |(CMD_ACCEPT name') => if Set.member(!fun_set,name') then raise Warning "Function already added."
				  else (addFun_intern (name',param,result,body) b; 
					fun_set := Set.insert(!fun_set,name');
					true))
    handle Warning s => (print ("Warning: Ignoring function "^name^" Reason: "^s^"\n"); false)
	    
    fun addFun p = addFun' p true
    fun addFun1 p = addFun' p false

    (* Fügt eine Konstante zum Container hinzu *)

    fun addConst_intern typ (name,value) =
	(add_sign(name,ENUMREF typ);
	 add_component_const(name,value);
	 update())

    fun addConst typ (name,value) =
	(case Config.applyFilter name of
	     CMD_REJECT => ((*print ("Filter: "^ name ^ " rejected.\n");*) false)
	   |(CMD_ACCEPT name') => if Set.member(!fun_set,name') then raise Warning "Value already added."
				  else (addConst_intern typ (name',value);
				      fun_set := Set.insert(!fun_set,name');
				      true))

    (* Fügt einen neuen Typ zum Container hinzu *)

    fun addType_intern (name, tyvar, place) = 
	(add_sign_string("\t\ttype "^(if tyvar then "'a" else "")^" "^name^
			 (if place <> "" then " = "^(if tyvar then "'a" else "")^" "^place^"."^name else "")^"\n");
	 update())

    fun addType(name,tyvar,place) = 
	(case Config.applyFilter name of
	     CMD_REJECT => ((*print ("Filter: "^ name ^ " rejected.\n");*) false)
	   |(CMD_ACCEPT name') => if Set.member(!type_set,name') then (print "Type already added.\n"; false) (* Keine exception werfen !!!! *)
				  else (addType_intern (name',tyvar,place);
					type_set := Set.insert(!type_set,name');
					true))

    (* Fuegt einen neuen datatype zum Container hinzu *)
 
    fun addEnum(name,tr:(string*int) list) = 
	let 
	    fun filter_names (n,i) = 
		(case Config.applyFilter n of
		     CMD_REJECT => NONE
		   |(CMD_ACCEPT name') => if i < 0 orelse i >= 1024 then NONE else SOME (name',i))

	    val tr' = mapPartial filter_names tr

	    val name_list = map #1 tr'
	    val map1 = map #2 tr'
      
	    (* Rücktransformation erstellen *)
	    val max = maxInt map1
	    val arr = Array.array(max+1,0);
     
	    fun write arr i nil = ()
	      | write arr i (x::xr) = (Array.update(arr,x,i); 
				       write arr (i+1) xr)
   
	    val _ = write arr 0 map1
	    val map2 = ref nil : int list ref
	    val _ = Array.app (fn x => map2 := x::(!map2)) arr
	    val _ = map2 := rev (!map2)
	in
	    if length map1 = 0 then (print ("Enum " ^ name ^ " rejected. Empty after filtering. \n"); false)
	    else case Config.applyFilter name of
		CMD_REJECT => ((*print ("Filter: "^ name ^ " rejected.\n");*) false)
	      |(CMD_ACCEPT name') => if Set.member(!type_set,name') then raise Warning "Value already added."
				     else (add_sign_string ("\n\t\tdatatype "^name'^" = "^(Util.concat(name_list," | ")));
					   add_binding_string("static int "^name^"_map1["^(Int.toString(length map1))^"] = {"^
							      concat((map Int.toString map1),",")^"};\n"^
							      "static int "^name^"_map2["^(Int.toString (max+1))^"] = {"^
							      concat((map Int.toString (!map2)),",")^"};\n");
					   update();
					   type_set := Set.insert(!type_set,name');
					   true)
	end

    (* Schreibt eine Liste *)
    fun WriteList(file,nil) = ()
      | WriteList(file,x::xr) = (TextIO.output(file,x^"\n"); WriteList(file,xr))

    (* Fügt eine Importzeile in die Signaturdatei ein *)
    fun handle_import_struct (n,src) =
	"import structure "^n^" from \""^src^"\"\n"

    fun handle_import_sig (n,src) =
	"import signature "^n^" from \""^src^"\"\n"

    (* Erstellt die Signaturdatei *)
    fun writeSignature name imports_struct imports_sig sign sig_file asig_file = 
	(TextIO.output(sig_file,
		       (concat(map handle_import_struct imports_struct,"\n"))^
		       (concat(map handle_import_sig imports_sig,"\n"))^
		       "signature " ^ name ^ " =\nsig\n");
	 WriteList(sig_file,sign);
	 TextIO.output(sig_file,"end\n");

	 TextIO.output(asig_file,
		       (concat(map handle_import_sig [(name,name^"-sig")],"\n"))^
		       "signature " ^ name ^ " =\nsig\n"^
		       "\tstructure "^name^": "^name^"\n");
	 TextIO.output(asig_file,"end\n"))

    (* Erstellt die Binding Datei *)
    fun writeBinding name headers binding component bind_file = 
	(TextIO.output(bind_file,
		       "#include <stdio.h>\n"^
		       "\n#include \"Authoring.hh\"\n\n"^
		       (concat(map (fn x => "#include \""^x^"\"\n") headers,"\n"))^
		       "\n///////////////////////////////////////////////////////////\n\n");
	 WriteList(bind_file,binding);
	 TextIO.output(bind_file,
		       "\n///////////////////////////////////////////////////////////\n\n"^
		       "word InitComponent()\n{\n\tRecord* record = Record::New("^(Int.toString(length component))^");\n\n");
	 WriteList(bind_file,component);
	 TextIO.output(bind_file,
		       "\n\tRETURN_STRUCTURE(\""^name^"$\",record);\n}\n"))

    (* Speichert das Binding ab *)
    fun saveBinding includes imports_struct imports_sig = 
      let 
	  val name = getName()

	  (* Singatur erstellen *)
	  val asig_file = TextIO.openOut (name^".asig")
	  val sig_file = TextIO.openOut (name^"-sig.aml")
	  val _ = writeSignature name imports_struct imports_sig (rev(!sign)) sig_file asig_file
	  val _ =  TextIO.closeOut sig_file
	  val _ =  TextIO.closeOut asig_file
	  		
	  (* Binding erstellen *)
	  val bind_file = TextIO.openOut (name^"-binding.cc")
	  val _ = writeBinding name includes (rev(!binding)) (rev(!component)) bind_file
	  val _ = TextIO.closeOut bind_file
	  		
      in () end

end

