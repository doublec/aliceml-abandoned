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

signature SIMPLIFY_MATCH =
    sig
	structure I: INTERMEDIATE_GRAMMAR = IntermediateGrammar
	structure O: FLAT_GRAMMAR = FlatGrammar

	datatype selector =
	    LABEL of Label.t
	  | LONGID of Stamp.t * Label.t list
	type pos = selector list

	datatype test =
	    LitTest of I.lit
	  | TagTest of Label.t * int * unit O.conArgs * O.conArity
	  | ConTest of I.longid * unit O.conArgs * O.conArity
	  | RefTest
	  | TupTest of int
	  | ProdTest of Label.t vector
	    (* sorted, all labels distinct, no tuple *)
	  | VecTest of int
	  | GuardTest of mapping * I.exp
	  | DecTest of mapping * I.dec vector
	withtype mapping = (pos * I.id) list

	val longidToSelector: I.longid -> selector

	datatype testGraph =
	    Node of pos * test * testGraph ref * testGraph ref * nodeStatus ref
	  | Leaf of O.body * O.body option ref
	  | Default
	and nodeStatus =
	    Initial
	  | Raw of testGraph list * testGraph list
	  | Cooked of (pos * test) list * (pos * test) list
	  | Translated of O.body

	type consequent = Source.region * O.body option ref
	type mapping' = (pos * O.id) list

	val buildGraph: (Source.region * I.pat * O.body) vector * O.body ->
	    testGraph * consequent list

	val buildFunArgs: (Source.region * I.pat * O.body) vector * O.body ->
	    O.idDef O.args * testGraph * mapping' * consequent list
    end
