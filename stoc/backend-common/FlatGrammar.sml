(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999-2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure FlatGrammar :> FLAT_GRAMMAR =
    struct
	(* Annotations *)

	datatype livenessInfo =
	    Unknown
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

	datatype idDef =
	    IdDef of id
	  | Wildcard

	datatype funFlag =
	    PrintName of string
	  | AuxiliaryOf of stamp
	  | IsToplevel

	datatype con =
	    Con of id
	  | StaticCon of stamp

	datatype 'a args =
	    OneArg of 'a
	  | TupArgs of 'a vector
	  | ProdArgs of (label * 'a) vector
	    (* sorted, all labels distinct, no tuple *)

	type 'a conArgs = 'a args option

	datatype prod =
	    Tuple of int
	  | Product of label vector

	datatype stm =
	  (* the following may never be last *)
	    ValDec of stm_info * idDef * exp
	  | RefAppDec of stm_info * idDef * id
	  | TupDec of stm_info * idDef vector * id
	  | ProdDec of stm_info * (label * idDef) vector * id
	  (* the following must always be last *)
	  | RaiseStm of stm_info * id
	  | ReraiseStm of stm_info * id
	  | TryStm of stm_info * body * idDef * body
	  | EndTryStm of stm_info * body
	  | EndHandleStm of stm_info * body
	    (* all bodies of EndTryStm/EndHandleStm corresponding to an
	     * exception handler are identical (and - if necessary - are
	     * marked by a SharedStm node) *)
	  | TestStm of stm_info * id * tests * body
	  | SharedStm of stm_info * body * stamp   (* used at least twice *)
	  | ReturnStm of stm_info * exp
	  | IndirectStm of stm_info * body option ref
	  | ExportStm of stm_info * exp
	and tests =
	    LitTests of (lit * body) vector
	  | TagTests of (label * int * idDef args option * body) vector
	  | ConTests of (con * idDef args option * body) vector
	  | VecTests of (idDef vector * body) vector
	and exp =
	    LitExp of exp_info * lit
	  | PrimExp of exp_info * string
	  | NewExp of exp_info
	  | VarExp of exp_info * id
	  | TagExp of exp_info * label * int
	  | ConExp of exp_info * con
	  | TupExp of exp_info * id vector
	  | ProdExp of exp_info * (label * id) vector
	    (* sorted, all labels distinct, no tuple *)
	  | VecExp of exp_info * id vector
	  | FunExp of exp_info * stamp * funFlag list * idDef args * body
	  | PrimAppExp of exp_info * string * id vector
	  | VarAppExp of exp_info * id * id args
	  | TagAppExp of exp_info * label * int * id args
	  | ConAppExp of exp_info * con * id args
	  | RefAppExp of exp_info * id
	  | SelAppExp of exp_info * prod * label * int * id
	  | FunAppExp of exp_info * id * stamp * id args
	withtype body = stm list

	type sign = IntermediateGrammar.sign
	type component = (idDef * sign * Url.t) vector * (body * sign)
	type t = component

	fun freshId info = Id (info, Stamp.new (), Name.InId)

	fun infoStm (ValDec (info, _, _)) = info
	  | infoStm (RefAppDec (info, _, _)) = info
	  | infoStm (TupDec (info, _, _)) = info
	  | infoStm (ProdDec (info, _, _)) = info
	  | infoStm (RaiseStm (info, _)) = info
	  | infoStm (ReraiseStm (info, _)) = info
	  | infoStm (TryStm (info, _, _, _)) = info
	  | infoStm (EndTryStm (info, _)) = info
	  | infoStm (EndHandleStm (info, _)) = info
	  | infoStm (TestStm (info, _, _, _)) = info
	  | infoStm (SharedStm (info, _, _)) = info
	  | infoStm (ReturnStm (info, _)) = info
	  | infoStm (IndirectStm (info, _)) = info
	  | infoStm (ExportStm (info, _)) = info

	fun litEq (IntLit i1, IntLit i2) = i1 = i2
	  | litEq (WordLit w1, WordLit w2) = w1 = w2
	  | litEq (CharLit c1, CharLit c2) = c1 = c2
	  | litEq (StringLit s1, StringLit s2) = s1 = s2
	  | litEq (RealLit r1, RealLit r2) = Real.== (r1, r2)
	  | litEq (_, _) = raise Crash.Crash "IntermediateAux.litEq"
    end
