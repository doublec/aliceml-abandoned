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

signature SIMPLIFY_MATCH =
    sig
	structure I: INTERMEDIATE_GRAMMAR = IntermediateGrammar
	structure O: SIMPLIFIED_GRAMMAR = SimplifiedGrammar

	datatype test =
	    LitTest of I.lit
	  | ConTest of I.longid * bool   (* has args *)
	  | RefTest
	  | TupTest of int
	  | RecTest of string list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of string
	  | VecTest of int
	  | GuardTest of mapping * I.exp
	  | DecTest of mapping * I.info * I.dec list
	withtype mapping = (string list * I.id) list

	type pos = string list

	datatype testGraph =
	    Node of pos * test * testGraph ref * testGraph ref * nodeStatus ref
	  | Leaf of O.exp * O.exp option ref
	  | Default
	and nodeStatus =
	    Initial
	  | Count of int
	  | Checking
	  | Raw of testGraph list * testGraph list
	  | Cooked of (pos * test) list * (pos * test) list
	  | Optimized of (pos * test) list * (pos * test) list
	  | Simplified of O.exp

	val buildGraph: (I.info * I.pat * O.exp) list * O.exp ->
	    testGraph * (O.coord * O.exp option ref) list
    end
