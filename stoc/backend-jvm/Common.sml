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
	open Abbrev

	exception Mitch

	(* Name der aktuellen Klasse. Der Stack wird für verschachtelte Funktionen benötigt.
	 (für jede Funktion wird eine eigene Klasse erzeugt.) *)
	structure Class =
	    struct
		val stack = ref [""]
		val initial = ref ""

		fun setInitial name = ((stack := [name]); initial := name)
		fun getInitial () = (!initial)
		fun getLiteralName() = getInitial()^"classLiberal"
	    end

	(* Den Feldnamen zu einer Id bestimmen. Die Id kann eine beliebige Variable sein. *)
	fun fieldNameFromStamp stamp' = "field"^(Stamp.toString stamp')
	fun fieldNameFromId (Id(_,stamp',_)) = fieldNameFromStamp stamp'

	fun nameFromId (Id (_,stamp',InId)) = "unnamed"^(Stamp.toString stamp')
	  | nameFromId (Id (_,stamp',ExId name')) = name'^(Stamp.toString stamp')

	(* Den Stamp aus einer Id extrahieren. *)
	fun stampFromId (Id (_, stamp', _)) = stamp'

	(* alpha steht am Begin einer Methode *)
	val alpha = 0:label

	(* Lokales JVM-Register, in dem das Übersetzungsergebnis festgehalten wird. *)
	val mainpickle = ref (Stamp.new ()) (* stamp for main structure *)

	val _ = Compiler.Control.Print.printLength := 10000;
	val _ = Compiler.Control.Print.printDepth := 10000;
	val _ = SMLofNJ.Internals.GC.messages false

	(* Den Klassennamen einer Id bestimmen, die üblicherweise die Id eines formalen
	 Funktionsparameters ist. *)
	fun classNameFromStamp stamp' = Class.getInitial()^"class"^(Stamp.toString stamp')
	fun classNameFromId (Id (_,stamp',_)) = classNameFromStamp stamp'

	val dummyCoord:ImperativeGrammar.coord = Source.nowhere
	val dummyPos:Source.position = Source.nowhere
	val dummyInfo:ImperativeGrammar.info = (dummyCoord, ref NONE)

	(* Functionclosures are represented by Stamps.
	  This is the toplevel environment: *)
	 val toplevel = Stamp.new()

	 (* A dummy stamp/id we sometimes write but should never read *)
	 val illegalStamp = Stamp.new()
	 val illegalId = Id (dummyPos, illegalStamp, InId)

	 (* compiler options *)
	 val DEBUG = ref 0
	 val VERBOSE = ref 0
	 val OPTIMIZE = ref 0

	 (* Stamps and Ids for formal Method Parameters. *)
	 val thisstamp = Stamp.new ()
	 val parm1Stamp = Stamp.new ()
	 val parm2Stamp = Stamp.new ()
	 val parm3Stamp = Stamp.new ()
	 val parm4Stamp = Stamp.new ()
	 val parm1Id = Id (dummyPos, parm1Stamp, InId)
	 val parm2Id = Id (dummyPos, parm2Stamp, InId)
	 val parm3Id = Id (dummyPos, parm3Stamp, InId)
	 val parm4Id = Id (dummyPos, parm4Stamp, InId)
	 val parmIds = #[nil, [parm1Id],[parm1Id,parm2Id],
			 [parm1Id,parm2Id,parm3Id],
			 [parm1Id,parm2Id,parm3Id,parm4Id]]

	 (* Stamp and Id for 'this'-Pointer *)
	 val thisStamp = Stamp.new ()
	 val thisId = Id (dummyPos, thisStamp, InId)

	 datatype APPLY =
	     (* Goto (# of params, label) *)
	     GotoLabel of int * label
	  (* Invokevirtual recapply (# of params, code class, code position, code label)*)
	  | InvokeRecApply of int * stamp * int * label
	  (* Invokeinterface apply or apply0/2/3/4. (# of params) *)
	  | NormalApply of int

	 (* generate names for apply methods. *)
	fun applyName 1 = "apply"
	  | applyName parms =
	    "apply"^Int.toString (if parms <=4 then parms else 1)

	(* Structure for managing labels in JVM-methods *)
	structure Label =
	    struct
		(* the actual label number *)
		val labelcount = ref (0:label)

		fun new () =
		    (labelcount := !labelcount + 1;
		     !labelcount)

		fun toString lab =
		    "label"^Int.toString lab
	    end
    end
