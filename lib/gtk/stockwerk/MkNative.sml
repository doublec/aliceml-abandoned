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
  the GTK+ functions.
  Most of its functionality is outsourced into the MkNativeHelper functor.
*)

functor MkNative(structure TypeManager : TYPE_MANAGER
		 structure Special : SPECIAL
		 val space : Util.spaces) :> GENERATOR = 
    struct
	open TypeTree
	open TypeManager

	structure NH = MkNativeHelper(structure TypeManager = TypeManager
				      structure Special = Special
				      val space = space
				      val makeFieldFuns = false)
	open NH

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
	  | processItem _ = ( nil , nil )

	(* STRUCTURE ENTRY GENERATION *)
	fun makeStructureEntry (FUNC(funName,ret,arglist)) =
	    let
		val al = splitArgTypes arglist
	    in
		structEntryLine(funName,al,false) ::
		 (if numOuts(al,false)>0 
		     then [structEntryLine(funName,al,true)] 
		     else nil)
	    end
	  | makeStructureEntry _ = nil

        (* main function for creating native files *)
        fun create tree = NH.generate tree processItem makeStructureEntry
    end
