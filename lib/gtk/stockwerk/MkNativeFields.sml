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
  This functor generates the native structure (.cc) and signature (.asig) for
  the field accessor functions.
  Most of its functionality is outsourced into the MkNativeHelper functor.
*)

functor MkNativeFields(structure TypeManager : TYPE_MANAGER
		       structure Special : SPECIAL
		       val space : Util.spaces) :> GENERATOR = 
    struct
	open TypeTree
	open TypeManager

	structure NH = MkNativeHelper(structure TypeManager = TypeManager
				      structure Special = Special
				      val space = space
				      val makeFieldFuns = true)
	open NH

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
	    val isConst = 
		case mtype of 
		    STRING _ => true
		  | POINTER _ => true
		  | _ => false
	    val callLine =
		if get then
		    [if isConst then "const " else "",
		     getCType mtype, " ret = (static_cast<",
		     stype, ">(", svar,"))->", mname,";\n"]
		else
		    ["(static_cast<", stype, ">(", svar,"))->", mname, " = ",
		     if isConst then "reinterpret_cast<" else "static_cast<",
		     getCType mtype, ">(", mvar, ");\n"]
	in
	    wrapperEntry callLine (funName,ret,al,false)
	end

        (* SIGNATURE AND WRAPPER ENTRIES *)
	fun processItem (STRUCT (structName, members)) =
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
	fun makeStructureEntry (STRUCT(structName, members)) =
	    let
		fun fieldGetSetInit sname get (mname,mtype) =
		    let
			val (funName,ret,arglist) = 
			    makeFieldFun space (sname,mname,mtype,get)
		    in
			structEntryLine(funName,
					ret,
					splitArgTypesNoOuts arglist,false)
		    end
		val members' = List.filter checkStructMember members
	    in
		(map (fieldGetSetInit structName true)  members') @
		(map (fieldGetSetInit structName false) members')
	    end
	  | makeStructureEntry (UNION(unionName, members)) =
	    makeStructureEntry (STRUCT(unionName, members))
	  | makeStructureEntry _ = nil

        (* main function for creating native files *)
        fun create tree = NH.generate tree processItem makeStructureEntry
    end
