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

signature LAMBDA =
    sig
	type stamp = Stamp.t
	type id = FlatGrammar.id

	val markForPickling: stamp * stamp -> unit

	val setId: stamp * id -> unit
	val getId: stamp -> id
	val getLambda: stamp -> stamp

	val generatePickleFn: (stamp * JVMInst.instr list -> JVMInst.instr list) * stamp * JVMInst.instr list -> JVMInst.instr list
	val makePickleFields: stamp * JVMInst.field list -> JVMInst.field list

	val getClassStamp: stamp * int -> stamp

	val isInRecApply: stamp * int -> bool
	val argSize: 'a FlatGrammar.args -> int
	val insertRec: (id * FlatGrammar.exp) list -> unit

	val addToRecApply: JVMInst.instr list * stamp * int -> unit
	val invokeRecApply: stamp * int -> Common.APPLY
	val buildRecApply: stamp * JVMInst.instr list -> JVMInst.method
	val showRecApplies: unit -> unit

	val setParmStamp: stamp * stamp * stamp -> unit
	val getParmStamp: stamp * stamp -> stamp
    end
