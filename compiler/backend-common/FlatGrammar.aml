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

structure FlatGrammar: FLAT_GRAMMAR =
    (*--** the above signature constraint should be opaque, but SML/NJ bombs *)
    struct
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

	datatype id = datatype IntermediateGrammar.id

	datatype conArity =
	    Nullary
	  | Unary
	  | Tuple of int
	  | Record of label list

	datatype funFlag =
	    PrintName of string
	  | AuxiliaryOf of stamp
	  | IsToplevel

	datatype 'a args =
	    OneArg of 'a
	  | TupArgs of 'a list
	  | RecArgs of (label * 'a) list
	    (* sorted, all labels distinct, no tuple *)

	datatype test =
	    LitTest of lit
	  | TagTest of label
	  | TagAppTest of label * id args * conArity
	    (* args may only be TupArgs if conArity is Tuple;
	     * args may only be RecArgs if conArity is Record *)
	  | ConTest of id
	  | ConAppTest of id * id args * conArity
	    (* args may only be TupArgs if conArity is Tuple;
	     * args may only be RecArgs if conArity is Record *)
	  | RefAppTest of id
	  | TupTest of id list
	  | RecTest of (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of label * id
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
	  | TagExp of exp_info * label * conArity
	  | ConExp of exp_info * id * conArity
	  | StaticConExp of exp_info * stamp * conArity
	  | RefExp of exp_info
	  | TupExp of exp_info * id list
	  | RecExp of exp_info * (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of exp_info * label
	  | VecExp of exp_info * id list
	  | FunExp of exp_info * stamp * funFlag list * id args * body
	  | PrimAppExp of exp_info * string * id list
	  | VarAppExp of exp_info * id * id args
	  | TagAppExp of exp_info * label * id args * conArity
	    (* args may only be TupArgs if conArity is Tuple;
	     * args may only be RecArgs if conArity is Record *)
	  | ConAppExp of exp_info * id * id args * conArity
	    (* args may only be TupArgs if conArity is Tuple;
	     * args may only be RecArgs if conArity is Record *)
	  | StaticConAppExp of exp_info * stamp * id args * conArity
	    (* args may only be TupArgs if conArity is Tuple;
	     * args may only be RecArgs if conArity is Record *)
	  | RefAppExp of exp_info * id
	  | SelAppExp of exp_info * label * id
	  | FunAppExp of exp_info * id * stamp * id args
	  | AdjExp of exp_info * id * id
	withtype body = stm list

	type sign = IntermediateGrammar.sign
	type component = (id * sign * Url.t) list * (body * sign)
	type t = component

	fun infoStm (ValDec (info, _, _)) = info
	  | infoStm (RecDec (info, _)) = info
	  | infoStm (EvalStm (info, _)) = info
	  | infoStm (RaiseStm (info, _)) = info
	  | infoStm (ReraiseStm (info, _)) = info
	  | infoStm (HandleStm (info, _, _, _, _, _)) = info
	  | infoStm (EndHandleStm (info, _)) = info
	  | infoStm (TestStm (info, _, _, _)) = info
	  | infoStm (SharedStm (info, _, _)) = info
	  | infoStm (ReturnStm (info, _)) = info
	  | infoStm (IndirectStm (info, _)) = info
	  | infoStm (ExportStm (info, _)) = info
    end
