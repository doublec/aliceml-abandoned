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

signature SIMPLIFIED =
    sig
	type coord = int * int

	(* Literals *)

	datatype lit =
	    WordLit of word
	  | IntLit of int
	  | CharLit of char
	  | StringLit of string
	  | RealLit of string

	(* Identifiers *)

	type stamp = int
	datatype name =
	    ExId of string
	  | InId
	datatype id = Id of coord * stamp * name

	datatype longid =
	    ShortId of coord * id
	  | LongId of coord * longid * id

	datatype lab = Lab of coord * string

	(* Generic *)

	type backendInfo
	val backendInfoDummy: backendInfo

	type shared = backendInfo ref

	(* Expressions and Declarations *)

	datatype dec =
	    ValDec of coord * id * exp
	  | ValRecDec of coord * id list * exp   (* all ids distinct *)
	  | ConDec of coord * id * bool   (* has args *)
	and exp =
	    LitExp of coord * lit
	  | VarExp of coord * longid
	  | ConExp of coord * longid * longid option
	  | TupExp of coord * longid list
	  | RecExp of coord * (lab * longid) list   (* all labels distinct *)
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
	  | RecTest of (string * id) list   (* sorted, distinct *)
	  | LabelTest of string * id
    end
