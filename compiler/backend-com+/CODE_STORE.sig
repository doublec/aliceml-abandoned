(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(*
 * The procedures in this structure may be called according to
 * the following grammar:
 *
 *    <file> ::= init { <method> } close
 *
 *    <method> ::= defineClass
 *              |  defineMethod { <method> } closeMethod
 *              |  emit
 *              |  emitId
 *              |  declareLocal
 *              |  kill
 *              |  saveRegState { <method> } restoreRegState
 *
 * Methods may be defined in classes before the corresponding
 * invocation of defineClass.  All classes must have been defined
 * when calling close.
 *)

signature CODE_STORE =
    sig
	type class = FlatGrammar.stamp

	val className: class -> IL.dottedname

	type savedRegState

	val init: IL.dottedname -> unit
	val defineClass: class * IL.extends * IL.implements -> unit
	val defineMethod: class * IL.id * FlatGrammar.id list -> unit
	val emit: IL.instr -> unit
	val emitId: FlatGrammar.id -> unit
	val declareLocal: FlatGrammar.id -> unit
	val kill: StampSet.t -> unit
	val saveRegState: unit -> savedRegState
	val restoreRegState: savedRegState -> unit
	val closeMethod: unit -> unit
	val close: unit -> IL.program
    end
