(*
 * Author:
 *   Andy Walter <anwalt@ps.uni-sb.de>
 *
 * Copyright:
 *   Andy Walter, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Common=
    struct
	(* Intermediate Representation: *)
	open IntermediateGrammar
	open ImperativeGrammar
	open Prebound
	open Main

	open JVMInst

	(* Name of the initial class *)
	structure Class =
	    struct
		val initial = ref ""

		fun setInitial name = initial := name
		fun getInitial () = !initial
	    end

	(* return the fieldname of an Id. Id may be any variable. *)
	fun fieldNameFromStamp stamp' = "field"^(Stamp.toString stamp')
	fun fieldNameFromId (Id(_,stamp',_)) = fieldNameFromStamp stamp'

	fun nameFromId (Id (_,stamp',InId)) = "unnamed"^(Stamp.toString stamp')
	  | nameFromId (Id (_,stamp',ExId name')) = name'^(Stamp.toString stamp')

	(* extract the stamp from an Id *)
	fun stampFromId (Id (_, stamp', _)) = stamp'

	val _ = Compiler.Control.Print.printLength := 10000;
	val _ = Compiler.Control.Print.printDepth := 10000;
	val _ = SMLofNJ.Internals.GC.messages false

	(* Functionclosures are represented by Stamps.
	 This is the toplevel environment: *)
	val toplevel = Stamp.new()

	(* return the class name of an Id. *)
	fun classNameFromStamp stamp' = Class.getInitial()^
	    (if stamp'=toplevel then "" else "class"^(Stamp.toString stamp'))
	fun classNameFromId (Id (_,stamp',_)) = classNameFromStamp stamp'

	val dummyCoord:ImperativeGrammar.coord = Source.nowhere
	val dummyPos:Source.position = Source.nowhere
	val dummyInfo:ImperativeGrammar.info = (dummyPos, ref Unknown)

	 (* A dummy stamp/id we sometimes write but should never read *)
	 val illegalStamp = Stamp.new()
	 val illegalId = Id (dummyPos, illegalStamp, InId)

	 (* compiler options *)
	 val DEBUG = ref 0
	 val VERBOSE = ref 0
	 val OPTIMIZE = ref 0
	 val LINES = ref false
	 val LMAA = ref false
	 val WAIT = ref false

	 (* Stamps and Ids for formal Method Parameters. *)
	 val thisstamp = Stamp.new ()
	 val parm1Stamp = Stamp.new ()
	 val parm2Stamp = Stamp.new ()
	 val parm3Stamp = Stamp.new ()
	 val parm4Stamp = Stamp.new ()
	 val parm5Stamp = Stamp.new ()
	 val parm1Id = Id (dummyPos, parm1Stamp, InId)
	 val parm2Id = Id (dummyPos, parm2Stamp, InId)
	 val parm3Id = Id (dummyPos, parm3Stamp, InId)
	 val parm4Id = Id (dummyPos, parm4Stamp, InId)
	 val parm5Id = Id (dummyPos, parm5Stamp, InId)
	 val parmIds = #[nil, [parm1Id],[parm1Id,parm2Id],
			 [parm1Id,parm2Id,parm3Id],
			 [parm1Id,parm2Id,parm3Id,parm4Id],
			 [parm1Id,parm2Id,parm3Id,parm4Id,parm5Id]]

	 (* Stamp and Id for 'this'-Pointer *)
	 val thisStamp = Stamp.new ()
	 val thisId = Id (dummyPos, thisStamp, InId)

	 datatype APPLY =
	  (* Invokevirtual recapply (# of params, code class, code position, code label)*)
	     InvokeRecApply of int * stamp * int * label
	  (* Invokeinterface apply or apply0/2/3/4. (# of params) *)
	  | NormalApply of int

	(* Structure for managing labels in JVM-methods *)
	structure Label =
	    struct
		(* the actual label number *)
		val labelcount = ref (0:label)

		val retryStack = ref [(0:label, toplevel)]

		fun new () =
		    (labelcount := !labelcount + 1;
		     !labelcount)

		fun toString lab =
		    "label"^Int.toString lab

		fun popRetry () =
		    let
			val ret = case !retryStack of
			    (l,_)::_ => l
			  | nil => Crash.crash "Label.popRetry"
		    in
			retryStack := tl (!retryStack);
			ret
		    end

		fun newRetry (ls as (lab', stamp')) =
		    case !retryStack of
			(_,stamp'')::_ => if stamp' = stamp'' then ()
				       else retryStack := ls :: (!retryStack)
		      | nil => Crash.crash "Label.newRetry"

		fun printStackTrace () =
		    let
			fun prstack ((l,s)::xs) = (print (toString l^":"^Stamp.toString s^"\n");
						   prstack xs)
			  | prstack nil = ()
		    in
			print "LabelStack:\n";
			prstack (!retryStack)
		    end
	    end
    end
