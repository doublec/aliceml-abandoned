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

structure Simplified :> SIMPLIFIED =
    struct
	type coord = Source.position

	(* Literals *)

	datatype lit = datatype PostTranslationIntermediate.lit

	(* Identifiers *)

	type stamp = PostTranslationIntermediate.stamp
	datatype name = datatype PostTranslationIntermediate.name
	datatype lab = datatype PostTranslationIntermediate.lab
	datatype id = datatype PostTranslationIntermediate.id
	datatype longid = datatype PostTranslationIntermediate.longid

	(* Generic *)

	type backendInfo = int   (*--** *)
	val backendInfoDummy = 0   (*--** *)

	type shared = backendInfo ref

	(* Expressions and Declarations *)

	datatype dec =
	    ValDec of coord * id * exp
	  | ValRecDec of coord * id list * exp * bool
	    (* all ids distinct, is recursive *)
	  | ConDec of coord * id * bool   (* has args *)
	and exp =
	    LitExp of coord * lit
	  | VarExp of coord * longid
	  | ConExp of coord * longid * longid option
	  | TupExp of coord * longid list
	  | RecExp of coord * (lab * longid) list
	    (* sorted, all labels distinct, no tuple *)
	  | SelExp of coord * lab
	  | FunExp of coord * string * id * exp
	  | AppExp of coord * exp * exp
	  | AdjExp of coord * exp * exp
	  | WhileExp of coord * exp * exp
	  | SeqExp of coord * exp list
	  | TestExp of coord * longid * test * exp * exp
	  | RaiseExp of coord * exp
	  | HandleExp of coord * exp * id * exp
	  | LetExp of coord * dec list * exp
	  | SharedExp of coord * exp * shared
	and test =
	    LitTest of lit
	  | NameTest of longid
	  | ConTest of longid * id
	  | RecTest of (string * id) list   (* sorted, all labels distinct *)
	  | LabelTest of string * id
    end
