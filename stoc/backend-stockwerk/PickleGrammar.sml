(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure PickleGrammar :> PICKLE_GRAMMAR =
    struct
	type sign = FlatGrammar.sign

	type id = int

	datatype idDef =
	    IdDef of id
	  | Wildcard

	datatype idRef =
	    Local of id
	  | Global of int

	datatype 'a args =
	    OneArg of 'a
	  | TupArgs of 'a vector

	datatype value =
	    Prim of string
	  | Int of LargeInt.int
	  | String of String.string
	  | WideString of WideString.string
	  | Real of Real.real
	  | Constructor of Stamp.t
	  | Tuple of value vector
	  | Vector of value vector
	  | Closure of function * value vector
	  | Sign of sign
	and function = Function of int * int * idDef args * instr
	and instr =
	    Kill of id vector * instr
	  | PutConst of id * value * instr
	  | PutVar of id * idRef * instr
	  | PutNew of id * instr
	  | PutTag of id * int * idRef vector * instr
	  | PutCon of id * con * idRef vector * instr
	  | PutRef of id * idRef * instr
	  | PutTup of id * idRef vector * instr
	  | PutVec of id * idRef vector * instr
	  | PutFun of id * idRef vector * function * instr
	  | AppPrim of string * idRef vector * (idDef * instr) option
	  | AppVar of idRef * idRef args * (idDef args * instr) option
	  | AppConst of value * idRef args * (idDef args * instr) option
	  | GetRef of id * idRef * instr
	  | GetTup of idDef vector * idRef * instr
	  | Raise of idRef
	  | Reraise of idRef
	  | Try of instr * idDef * idDef * instr
	  | EndTry of instr
	  | EndHandle of instr
	  | IntTest of idRef * (Int32.int * instr) vector * instr
	  | RealTest of idRef * (Real.real * instr) vector * instr
	  | StringTest of idRef * (String.string * instr) vector * instr
	  | WideStringTest of idRef * (WideString.string * instr) vector
				    * instr
	  | TagTest of idRef * (int * instr) vector
			     * (int * idDef vector * instr) vector * instr
	  | ConTest of idRef * (con * instr) vector
			     * (con * idDef vector * instr) vector * instr
	  | VecTest of idRef * (idDef vector * instr) vector * instr
	  | Shared of Stamp.t * instr
	  | Return of idRef args
	and con =
	    Con of idRef
	  | StaticCon of value

	type t = value
    end
