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
	type typ = Type.t

	(*--** These do not need to store the type any more: *)
	datatype test =
	    LitTest of I.lit
	  | TagTest of Label.t * int
	  | TagAppTest of Label.t * int * typ O.args * O.conArity
	  | ConTest of I.longid
	  | ConAppTest of I.longid * typ O.args * O.conArity
	  | RefAppTest of typ
	  | TupTest of typ list
	  | ProdTest of (Label.t * typ) list
	    (* sorted, all labels distinct, no tuple *)
	  | LabTest of Label.t * int * typ
	  | VecTest of typ list
	  | GuardTest of mapping * I.exp
	  | DecTest of mapping * I.dec list
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

	val buildGraph: (Source.region * I.pat * O.body) list * O.body ->
	    testGraph * consequent list

	val buildFunArgs: (Source.region * I.pat * O.body) list * O.body ->
	    O.idDef O.args * testGraph * mapping * consequent list
    end
