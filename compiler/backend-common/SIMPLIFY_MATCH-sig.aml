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
	structure O: IMPERATIVE_GRAMMAR = ImperativeGrammar

	type pos = Label.t list

	datatype test =
	    LitTest of I.lit
	  | ConTest of I.longid * bool   (* has args *)
	  | RefTest
	  | TupTest of int
	  | RecTest of Label.t list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of Label.t
	  | VecTest of int
	  | GuardTest of mapping * I.exp
	  | DecTest of mapping * I.info * I.dec list
	withtype mapping = (pos * I.id) list

	datatype testGraph =
	    Node of pos * test * testGraph ref * testGraph ref * nodeStatus ref
	  | Leaf of O.body * O.body option ref
	  | Default
	and nodeStatus =
	    Initial
	  | Raw of testGraph list * testGraph list
	  | Cooked of (pos * test) list * (pos * test) list
	  | Optimized of (pos * test) list * (pos * test) list
	  | Translated of O.body

	type consequent = (O.coord * O.body option ref)

	val buildGraph: (I.info * I.pat * O.body) list * O.body ->
	    testGraph * consequent list

	type bodyFun = unit -> O.body

	val buildFunArgs: I.id * (I.info * I.pat * O.body) list * bodyFun ->
	    (O.id O.args * testGraph * mapping * consequent list) list
    end
