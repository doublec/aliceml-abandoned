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

structure ImperativeGrammar :> IMPERATIVE_GRAMMAR =
    struct
	type coord = Source.position

	(* Literals *)

	datatype lit = datatype IntermediateGrammar.lit

	(* Identifiers *)

	datatype name = datatype IntermediateGrammar.name
	datatype lab = datatype IntermediateGrammar.lab
	datatype id = datatype IntermediateGrammar.id

	(* Expressions and Declarations *)

	type shared = int ref

	type isToplevel = bool
	type hasArgs = bool

	datatype test =
	    LitTest of lit
	  | ConTest of id * id option
	  | TupTest of id list
	  | RecTest of (string * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of string * id
	  | VecTest of id list

	datatype 'a args =
	    OneArg of 'a
	  | TupArgs of 'a list
	  | RecArgs of (string * 'a) list
	    (* sorted, all labels distinct, no tuple *)

	datatype stm =
	    ValDec of coord * id * exp * isToplevel
	  | RecDec of coord * (id * exp) list * isToplevel
	    (* all ids distinct *)
	  | ConDec of coord * id * hasArgs * isToplevel
	  | EvalStm of coord * exp
	  (* the following must always be last *)
	  | HandleStm of coord * body * id * body
	  | EndHandleStm of coord * body
	  | TestStm of coord * id * test * body * body
	  | RaiseStm of coord * id
	  | SharedStm of coord * body * shared   (* used at least twice *)
	  | ReturnStm of coord * exp
	  | IndirectStm of coord * body option ref
	  | ExportStm of coord * id list
	and exp =
	    LitExp of coord * lit
	  | PrimExp of coord * string
	  | VarExp of coord * id
	  | ConExp of coord * id * hasArgs
	  | TupExp of coord * id list
	  | RecExp of coord * (lab * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of coord * lab
	  | VecExp of coord * id list
	  | FunExp of coord * string * (id args * body) list
	    (* all arities distinct; always contains a single OneArg *)
	  | AppExp of coord * id * id args
	  | SelAppExp of coord * lab * id
	  | ConAppExp of coord * id * id
	  | PrimAppExp of coord * string * id list
	  | AdjExp of coord * id * id
	withtype body = stm list

	type program = body
    end
