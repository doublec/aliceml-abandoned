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
	type coord = Source.position

	(* Literals *)

	datatype lit = datatype IntermediateGrammar.lit

	(* Identifiers *)

	type stamp = IntermediateGrammar.stamp

	datatype name = datatype IntermediateGrammar.name
	datatype id = datatype IntermediateGrammar.id

	type lab = string

	(* Expressions and Declarations *)

	type shared = int ref

	type isToplevel = bool
	type hasArgs = bool

	datatype test =
	    LitTest of lit
	  | ConTest of id * id option
	  | RefTest of id
	  | TupTest of id list
	  | RecTest of (lab * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of lab * id
	  | VecTest of id list

	datatype funFlag =
	    PrintName of string
	  | AuxiliaryOf of stamp

	datatype 'a args =
	    OneArg of 'a
	  | TupArgs of 'a list
	  | RecArgs of (lab * 'a) list
	    (* sorted, all labels distinct, no tuple *)

	structure StampSet =
	    MakeHashImpSet(type t = stamp
			   val hash = Stamp.hash)

	type info = coord * StampSet.t option ref

	datatype stm =
	    ValDec of info * id * exp * isToplevel
	  | RecDec of info * (id * exp) list * isToplevel
	    (* all ids distinct *)
	  | ConDec of info * id * hasArgs * isToplevel
	  | EvalStm of info * exp
	  | RaiseStm of info * id
	  (* the following must always be last *)
	  | HandleStm of info * body * id * body * body * shared
	  | EndHandleStm of info * shared
	  | TestStm of info * id * test * body * body
	  | SharedStm of info * body * shared   (* used at least twice *)
	  | ReturnStm of info * exp
	  | IndirectStm of info * body option ref
	  | ExportStm of info * id list
	and exp =
	    LitExp of coord * lit
	  | PrimExp of coord * string
	  | VarExp of coord * id
	  | ConExp of coord * id * hasArgs
	  | RefExp of coord
	  | TupExp of coord * id list
	  | RecExp of coord * (lab * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of coord * lab
	  | VecExp of coord * id list
	  | FunExp of coord * funFlag list * (id args * body) list
	    (* all arities distinct; always contains a single OneArg *)
	  | AppExp of coord * id * id args
	  | SelAppExp of coord * lab * id
	  | ConAppExp of coord * id * id args
	  | RefAppExp of coord * id args
	  | PrimAppExp of coord * string * id list
	  | AdjExp of coord * id * id
	withtype body = stm list

	type program = body

	fun infoStm (ValDec (info, _, _, _)) = info
	  | infoStm (RecDec (info, _, _)) = info
	  | infoStm (ConDec (info, _, _, _)) = info
	  | infoStm (EvalStm (info, _)) = info
	  | infoStm (RaiseStm (info, _)) = info
	  | infoStm (HandleStm (info, _, _, _, _, _)) = info
	  | infoStm (EndHandleStm (info, _)) = info
	  | infoStm (TestStm (info, _, _, _, _)) = info
	  | infoStm (SharedStm (info, _, _)) = info
	  | infoStm (ReturnStm (info, _)) = info
	  | infoStm (IndirectStm (info, _)) = info
	  | infoStm (ExportStm (info, _)) = info
    end
