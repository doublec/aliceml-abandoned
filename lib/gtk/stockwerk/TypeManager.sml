
structure TypeManager :> TYPE_MANAGER =
struct
    open TypeTree
	
    datatype argtype = IN | OUT
    type arginfo = argtype * string * TypeTree.ty

    fun removeTypeRefs (POINTER t)     = POINTER (removeTypeRefs t)
      | removeTypeRefs (ARRAY (x,t))   = ARRAY(x, removeTypeRefs t)
      | removeTypeRefs (TYPEREF (_,t)) = removeTypeRefs t
      | removeTypeRefs t               = t

    local
	fun numericToCType sign kind =
	    (if sign then "" else "unsigned ")^
		 (case kind of
		      CHAR => "char" 
		    | SHORT => "short" 
		    | INT => "int" 
		    | LONG => "long" 
		    | LONGLONG => "longlong" 
		    | FLOAT => "float" 
		    | DOUBLE => "double" 
		    | LONGDOUBLE => "long double")
    in
	fun getCType VOID                     = "void"
	  | getCType (ELLIPSES true)          = "..."
	  | getCType (ELLIPSES false)         = "va_list"
	  | getCType BOOL                     = "gboolean"
	  | getCType (NUMERIC (sign,_,kind))  = numericToCType sign kind
	  | getCType (POINTER t)              = (getCType t)^"*"
	  | getCType (STRING false)           = "guchar*"
	  | getCType (STRING true)            = "gchar*"
	  | getCType (ARRAY (SOME i, t))      = 
	    (getCType t)^"["^LargeInt.toString(i)^"]"
	  | getCType (ARRAY (NONE, t))        = (getCType t)^"[]"
	  | getCType (LIST (name,_))          = name^"*"
	  | getCType (FUNCTION (ret,arglist)) = 
 	        getCType(ret)^"(*)("^
		(Util.makeTuple ", " "void" (map getCType arglist)) ^")"
	  | getCType (STRUCTREF name)   = name
	  | getCType (UNIONREF name)    = name
	  | getCType (ENUMREF name)     = name
	  | getCType (TYPEREF (name,_)) = name
    end

    fun getAliceType VOID                  = "unit"
      | getAliceType (ELLIPSES _)          = "arg list"
      | getAliceType BOOL                  = "bool"
      | getAliceType (NUMERIC (_,false,_)) = "int"
      | getAliceType (NUMERIC (_,true,_))  = "real"
      | getAliceType (POINTER _)           = "object"
      | getAliceType (STRING _)            = "string"
      | getAliceType (ARRAY (_,t))         = (getAliceType t) ^ " array"
      | getAliceType (LIST (_,t))          = (getAliceType t) ^ " list"
      | getAliceType (FUNCTION _)          = "object"
      | getAliceType (STRUCTREF _)         = raise EStruct
      | getAliceType (UNIONREF _)          = raise EUnion
      | getAliceType (ENUMREF name)        = name
      | getAliceType (TYPEREF (_,t))       = getAliceType t
	 
    fun getAliceNativeType t =
	let 
	    val s = getAliceType t
	in  
	    if (Util.checkPrefix "object" s orelse Util.checkPrefix "arg" s)
		then "'"^s 
	        else case removeTypeRefs t of (ENUMREF _) => "real" | _ => s
	end
      
    local
	fun isOutArg' (t as (POINTER (NUMERIC _)))      = true
	  | isOutArg' (POINTER (POINTER (STRUCTREF _))) = true
	  | isOutArg' (POINTER (ENUMREF _))             = true
	  | isOutArg' (POINTER (STRING _))              = true
	  | isOutArg' _                                 = false
    in
	fun isOutArg t = isOutArg' (removeTypeRefs t)
    end

    (* splits ret and arglist in inArgs and outArgs *)
    (* where outArgs lose their POINTERs *)
    fun splitArgTypes arglist =
    let
	val aname = fn IN => "in" | OUT => "out"
	val rp = fn (POINTER t) => t | t => t
	fun makeArg (t,num) = 
	let 
	    val at = if isOutArg t then OUT else IN
	in  
	    (at, (aname at) ^ num, if at=IN then t else rp t)
        end

       	val arglist' = List.filter (fn VOID => false | _ => true) arglist
    in
	ListPair.map makeArg (arglist', 
			      List.tabulate(length arglist',Int.toString))
    end

    fun splitInOuts (l,doinout) =
	(List.filter (fn (IN,_,_) => true | _ => doinout) l,
	 List.filter (fn (IN,_,_) => false | _ => true) l)
    fun numIns (l,doinout) = length (#1 (splitInOuts (l,doinout)))
    fun numOuts (l,doinout) = length (#2 (splitInOuts (l,doinout)))

    fun getCFunType (funName, ret, arglist, mask) =
    let
	fun getCType' (IN,_,t) = getCType t
	  | getCType' (OUT,_,t)  = getCType (POINTER t)
	val s = (getCType ret) ^ " " ^ funName ^ "(" ^ 
	        (Util.makeTuple ", " "void" (map getCType' arglist)) ^ ")"
    in
	if mask then (Util.replaceChar (#"*","#") s) else s
    end

    fun getAliceFunType (funName, ret, arglist, doinout) convFun =
    let
	val (ins,outs') = splitInOuts (arglist,doinout)
	val outs = if ret=VOID then outs' else (OUT, "", ret)::outs'
	fun getType (_,_,t) = t
    in
	"val "^funName^" : "^
	(Util.makeTuple " * " "unit" (map (convFun o getType) ins)) ^ " -> " ^
	(Util.makeTuple " * " "unit" (map (convFun o getType) outs))
    end

   
    local
	fun getClassList' nil                       cs = rev cs
	  | getClassList' (STRUCT (name,(_,t)::_)::dr) cs =
	    (case removeTypeRefs t of
		 STRUCTREF sup => 
		     ( 
		       print (name^" => "^sup^"\n") ;
		       getClassList' dr (CLASS (sup,name)::cs) )
	       | _             => getClassList' dr cs)
	  | getClassList' (_::dr)                   cs = getClassList' dr cs
    in
	fun getClassList ds = getClassList' ds nil
    end

    
    fun isRefOfSpace space (ENUMREF n) =
	Util.checkPrefix (Util.spaceName space) n
      | isRefOfSpace space (STRUCTREF n) =
	Util.checkPrefix (Util.spaceName space) n
      | isRefOfSpace space (TYPEREF (_,t)) = isRefOfSpace space t
      | isRefOfSpace _ _ = false

    fun isItemOfSpace space (FUNC (n,_,_)) = 
	Util.checkPrefix (Util.spaceFuncPrefix(space)) n
      | isItemOfSpace space (ENUM (n,_)) =
	Util.checkPrefix (Util.spaceEnumPrefix(space)) n
      | isItemOfSpace space (STRUCT (n,_)) =
	Util.checkPrefix (Util.spaceStructPrefix(space)) n
      | isItemOfSpace _ _ = false
	
    fun checkItem (FUNC (n,ret,arglist)) =
    let
	fun error s = ( print ("function "^n^" ignored: "^s^"\n") ; false )
    in
	( map getAliceType (ret::arglist) ; true )
	handle
	   EStruct   => error "struct in arglist or retval"
	 | EUnion    => error "union in arglist or retval"
      (* | EFunction => warning "function in arglist or retval"
	 | EEllipses => error "ellipses in argument list"
         | EArray    => warning "array in arglist or retval" *)
    end		    
      | checkItem _ = true


    fun getEnumSpace name = 
	foldl (fn (s,e) => if isRefOfSpace s (ENUMREF name) then s else e)
	       Util.GTK  Util.allSpaces

end

