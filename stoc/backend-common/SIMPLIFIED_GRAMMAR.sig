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

signature SIMPLIFIED_GRAMMAR =
    sig
	type coord = Source.position

	(* Literals *)

	datatype lit = datatype IntermediateGrammar.lit

	(* Identifiers *)

	type stamp = IntermediateGrammar.stamp
	datatype name = datatype IntermediateGrammar.name
	datatype lab = datatype IntermediateGrammar.lab
	datatype id = datatype IntermediateGrammar.id
	datatype longid = datatype IntermediateGrammar.longid

	(* Generic *)

	type backendInfo = int   (*--** *)
	val backendInfoDummy: backendInfo   (*--** *)

	type shared = backendInfo ref

	(* Expressions and Declarations *)

	datatype dec =
	    OneDec of coord * id * exp
	  | ValDec of coord * id list * exp   (* all ids distinct *)
	  | RecDec of coord * (id * exp) list   (* all ids distinct *)
	  | ConDec of coord * id * bool   (* has args *)
	and exp =
	    LitExp of coord * lit
	  | VarExp of coord * longid
	  | ConExp of coord * longid * longid option * bool
	  | TupExp of coord * longid list
	  | RecExp of coord * (lab * longid) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of coord * lab * exp option
	  | VecExp of coord * longid list
	  | FunExp of coord * string * (id args * exp) list
	    (* all arities distinct; always contains a single OneArg *)
	  | AppExp of coord * longid * exp * bool ref   (* is tail *)
	  | AdjExp of coord * exp * exp
	  | WhileExp of coord * exp * exp
	  | SeqExp of coord * exp list
	  | TestExp of coord * longid * test * exp * exp
	  | RaiseExp of coord * exp
	  | HandleExp of coord * exp * id * exp
	  | LetExp of coord * dec list * exp
	  | SharedExp of coord * exp * shared
	  | DecExp of coord * id list
	and test =
	    LitTest of lit
	  | ConTest of longid * id option
	  | TupTest of id list
	  | RecTest of (string * id) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of string * id
	  | VecTest of id list
	and 'a args =
	    OneArg of 'a
	  | TupArgs of 'a list
	  | RecArgs of (string * 'a) list
	    (* sorted, all labels distinct, no tuple *)

	type program = dec list * id list

	val coordOf: exp -> coord
    end
