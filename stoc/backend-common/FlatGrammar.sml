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

structure ImperativeGrammar: IMPERATIVE_GRAMMAR =
    (*--** the above signature constraint should be opaque *)
    struct
	type coord = Source.region

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

	type stmInfo = coord * livenessInfo ref
	type expInfo = IntermediateInfo.t

	datatype test =
	    LitTest of lit
	  | ConTest of id * id option * conArity
	  | RefTest of id
	  | TupTest of id list
	  | RecTest of (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of label * id
	  | VecTest of id list

	datatype stm =
	    ValDec of stmInfo * id * exp * isToplevel
	  | RecDec of stmInfo * (id * exp) list * isToplevel
	    (* all ids distinct *)
	  | EvalStm of stmInfo * exp
	  | RaiseStm of stmInfo * id
	  | ReraiseStm of stmInfo * id
	  (* the following must always be last *)
	  | HandleStm of stmInfo * body * id * body * body * shared
	  | EndHandleStm of stmInfo * shared
	  | TestStm of stmInfo * id * test * body * body
	  | SharedStm of stmInfo * body * shared   (* used at least twice *)
	  | ReturnStm of stmInfo * exp
	  | IndirectStm of stmInfo * body option ref
	  | ExportStm of stmInfo * exp
	and exp =
	    LitExp of expInfo * lit
	  | PrimExp of expInfo * string
	  | NewExp of expInfo * string option * conArity
	  | VarExp of expInfo * id
	  | ConExp of expInfo * id * conArity
	  | RefExp of expInfo
	  | TupExp of expInfo * id list
	  | RecExp of expInfo * (label * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of expInfo * label
	  | VecExp of expInfo * id list
	  | FunExp of expInfo * stamp * funFlag list * id args * body
	  | AppExp of expInfo * id * id args
	  | SelAppExp of expInfo * label * id
	  | ConAppExp of expInfo * id * id args * conArity
	  | RefAppExp of expInfo * id args
	  | PrimAppExp of expInfo * string * id list
	  | AdjExp of expInfo * id * id
	withtype body = stm list

	type sign = IntermediateGrammar.sign
	type component = (id * sign * Url.t) list * (body * sign)

	fun infoStm (ValDec (info, _, _, _)) = info
	  | infoStm (RecDec (info, _, _)) = info
	  | infoStm (EvalStm (info, _)) = info
	  | infoStm (RaiseStm (info, _)) = info
	  | infoStm (ReraiseStm (info, _)) = info
	  | infoStm (HandleStm (info, _, _, _, _, _)) = info
	  | infoStm (EndHandleStm (info, _)) = info
	  | infoStm (TestStm (info, _, _, _, _)) = info
	  | infoStm (SharedStm (info, _, _)) = info
	  | infoStm (ReturnStm (info, _)) = info
	  | infoStm (IndirectStm (info, _)) = info
	  | infoStm (ExportStm (info, _)) = info
    end
