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
	(* Annotations *)

	datatype livenessInfo =
	    Unknown
	  | LoopStart   (* internal *)
	  | LoopEnd   (* internal *)
	  | Use of StampSet.t   (* internal *)
	  | Kill of StampSet.t

	type id_info = {region: Source.region}
	type stm_info = {region: Source.region, liveness: livenessInfo ref}
	type exp_info = {region: Source.region}

	(* Statements and Expressions *)

	datatype lit = datatype IntermediateGrammar.lit

	type stamp = Stamp.t
	type name = Name.t
	type label = Label.t

	datatype id = Id of id_info * Stamp.t * Name.t

	datatype funFlag =
	    PrintName of string
	  | AuxiliaryOf of stamp
	  | IsToplevel

	datatype arity =
	    Unary
	  | TupArity of int
	  | RecArity of label list
	    (* sorted, all labels distinct, no tuple *)

	type conArity = arity option

	datatype 'a args =
	    OneArg of 'a
	  | TupArgs of 'a list
	  | RecArgs of (label * 'a) list
	    (* sorted, all labels distinct, no tuple *)

	datatype test =
	    LitTest of lit
	  | TagTest of label * int
	  | TagAppTest of label * int * id args
	  | ConTest of id
	  | ConAppTest of id * id args
	  | RefAppTest of id
	  | TupTest of id list
	  | RecTest of (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of label * int * id
	  | VecTest of id list

	datatype stm =
	    ValDec of stm_info * id * exp
	  | RecDec of stm_info * (id * exp) list
	    (* all ids distinct *)
	  | EvalStm of stm_info * exp
	  | RaiseStm of stm_info * id
	  | ReraiseStm of stm_info * id
	  (* the following must always be last *)
	  | HandleStm of stm_info * body * id * body * body * stamp
	  | EndHandleStm of stm_info * stamp
	  | TestStm of stm_info * id * (test * body) list * body
	  | SharedStm of stm_info * body * stamp   (* used at least twice *)
	  | ReturnStm of stm_info * exp
	  | IndirectStm of stm_info * body option ref
	  | ExportStm of stm_info * exp
	and exp =
	    LitExp of exp_info * lit
	  | PrimExp of exp_info * string
	  | NewExp of exp_info * conArity
	  | VarExp of exp_info * id
	  | TagExp of exp_info * label * int * conArity
	  | ConExp of exp_info * id * conArity
	  | StaticConExp of exp_info * stamp * conArity
	  | RefExp of exp_info
	  | TupExp of exp_info * id list
	  | RecExp of exp_info * (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of exp_info * label * int
	  | VecExp of exp_info * id list
	  | FunExp of exp_info * stamp * funFlag list * id args * body
	  | PrimAppExp of exp_info * string * id list
	  | VarAppExp of exp_info * id * id args
	  | TagAppExp of exp_info * label * int * id args
	  | ConAppExp of exp_info * id * id args
	  | StaticConAppExp of exp_info * stamp * id args
	  | RefAppExp of exp_info * id
	  | SelAppExp of exp_info * label * int * id
	  | FunAppExp of exp_info * id * stamp * id args
	withtype body = stm list

	type sign = IntermediateGrammar.sign
	type component = (id * sign * Url.t) list * (body * sign)
	type t = component

	val infoStm: stm -> stm_info
    end
