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

signature FLAT_GRAMMAR =
    sig
	(* Literals *)

	datatype lit = datatype IntermediateGrammar.lit

	(* Identifiers *)

	type stamp = Stamp.t
	type name = Name.t

	datatype id = datatype IntermediateGrammar.id

	type label = Label.t

	(* Expressions and Declarations *)

	type shared = int ref

	type isToplevel = bool

	datatype conArity =
	    Nullary
	  | Unary
	  | Tuple of int
	  | Record of label list

	datatype funFlag =
	    PrintName of string
	  | AuxiliaryOf of stamp

	datatype 'a args =
	    OneArg of 'a
	  | TupArgs of 'a list
	  | RecArgs of (label * 'a) list
	    (* sorted, all labels distinct, no tuple *)

	datatype livenessInfo =
	    Unknown
	  | LoopStart   (* internal *)
	  | LoopEnd   (* internal *)
	  | Use of StampSet.t   (* internal *)
	  | Kill of StampSet.t

	type id_info = IntermediateInfo.id_info
	type stm_info = {region: Source.region, liveness: livenessInfo ref}
	type exp_info = {region: Source.region, typ: Type.t}

	datatype test =
	    LitTest of lit
	  | TagTest of label * id option * conArity
	  | ConTest of id * id option * conArity
	  | RefTest of id
	  | TupTest of id list
	  | RecTest of (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of label * id
	  | VecTest of id list

	datatype stm =
	    ValDec of stm_info * id * exp * isToplevel
	  | RecDec of stm_info * (id * exp) list * isToplevel
	    (* all ids distinct *)
	  | EvalStm of stm_info * exp
	  | RaiseStm of stm_info * id
	  | ReraiseStm of stm_info * id
	  (* the following must always be last *)
	  | HandleStm of stm_info * body * id * body * body * shared
	  | EndHandleStm of stm_info * shared
	  | TestStm of stm_info * id * test * body * body
	  | SharedStm of stm_info * body * shared   (* used at least twice *)
	  | ReturnStm of stm_info * exp
	  | IndirectStm of stm_info * body option ref
	  | ExportStm of stm_info * exp
	and exp =
	    LitExp of exp_info * lit
	  | PrimExp of exp_info * string
	  | NewExp of exp_info * conArity
	  | VarExp of exp_info * id
	  | TagExp of exp_info * label * conArity
	  | ConExp of exp_info * id * conArity
	  | RefExp of exp_info
	  | TupExp of exp_info * id list
	  | RecExp of exp_info * (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of exp_info * label
	  | VecExp of exp_info * id list
	  | FunExp of exp_info * stamp * funFlag list * id args * body
	  | AppExp of exp_info * id * id args
	  | SelAppExp of exp_info * label * id
	  | TagAppExp of exp_info * label * id args * conArity
	  | ConAppExp of exp_info * id * id args * conArity
	  | RefAppExp of exp_info * id args
	  | PrimAppExp of exp_info * string * id list
	  | AdjExp of exp_info * id * id
	withtype body = stm list

	type sign = IntermediateGrammar.sign
	type component = (id * sign * Url.t) list * (body * sign)
	type t = component

	val infoStm: stm -> stm_info
    end
