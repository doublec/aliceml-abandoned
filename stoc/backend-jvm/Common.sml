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

	(* falls was böses passiert, wird eine Error-exception mit sinnvollem Inhalt 'geraist' *)
	exception Error of string

	exception Mitch

	(* xxx For Debugging: *)
	datatype deb=B of bool
	  | Is of id list
	  | Ias of id list array
	  | IasSS of id list array * int * int
	  | II of id * id
	  | Okay
	  | Dec of stm
	  | Test of test
	  | Exp of exp
	exception Debug of deb

	(* Name der aktuellen Klasse. Der Stack wird für verschachtelte Funktionen benötigt.
	 (für jede Funktion wird eine eigene Klasse erzeugt.) *)
	structure Class =
	    struct
		val stack = ref [""]
		val initial = ref ""

		fun getCurrent () = case !stack of (x::xs) => x | _ => raise Error("Class.getCurrent")
		fun push name = stack := name::(!stack)
		fun pop () =  case !stack of (x::xs) => stack := xs | _ => raise Error("Class.pop")
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

	(* Dieser Label steht am Ende der Registerinitialisierung von Methoden. *)
	val afterInit = "labelAfterInit"

	(* alpha steht am Begin einer Methode *)
	val alpha = "labelAlpha"

	(* Omega kommt vor dem abschliessenden Areturn *)
	val omega = "labelOmega"

	(* Lokales JVM-Register, in dem das Übersetzungsergebnis festgehalten wird. *)
	val mainpickle = ref ~1 (* JVM-Register, in dem Struktur steht *)

	val _ = Compiler.Control.Print.printLength := 10000;
	val _ = Compiler.Control.Print.printDepth := 10000;
	val _ = SMLofNJ.Internals.GC.messages false

	(* Den Klassennamen einer Id bestimmen, die üblicherweise die Id eines formalen
	 Funktionsparameters ist. *)
	fun classNameFromStamp stamp' = Class.getInitial()^"class"^(Stamp.toString stamp')
	fun classNameFromId (Id (_,stamp',_)) = classNameFromStamp stamp'

	val dummyCoord:ImperativeGrammar.coord = (0,0)
	val dummyPos:Source.position = (0,0)
    end
