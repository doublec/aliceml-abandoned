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

signature PICKLE =
    sig
	datatype value = ... (*--** missing *)

	datatype con =
	    Con of id
	  | StaticCon of value

	datatype 'a args =
	    OneArg of 'a
	  | TupArgs of 'a vector

	datatype function = Function of int * idDef args * instr

	datatype instr =
	    PutConst of id * value * instr
	  | PutNew of id * instr
	  | PutGlobal of id * int * instr
	  | PutTag of id * int * id vector * instr
	  | PutCon of id * con * id vector * instr
	  | PutRef of id * id * instr
	  | PutTup of id * id vector * instr
	  | PutSel of id * int * id * instr
	  | PutVec of id * id vector * instr
	  | PutFun of id * id vector * function * instr
	  | Kill of id vector * instr
	  | AppPrim of id * string * id vector * instr
	  | AppVar of id args * id * id args * instr
	  | GetTup of idDef vector * id * instr
	  | Try of instr * id * instr
	  | EndTry of instr
	  | EndHandle of instr
	  | IntTest of id * (int * instr) vector * instr
	  | RealTest of id * (real * instr) vector * instr
	  | StringTest of id * (string * instr) vector * instr
	  | TagTest of id * (int * instr) vector
			  * (int * idDef vector * instr) vector * instr
	  | ConTest of id * (con * instr) vector
			  * (con * idDef vector * instr) vector * instr
	  | VecTest of id * (idDef vector * instr) vector * instr
	  | Return of id args
    end
