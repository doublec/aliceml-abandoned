package de.uni_sb.ps.DML.DMLBuiltin;

import de.uni_sb.ps.DML.DMLRuntime.*;

final public class List {
    // exception Empty
    final public static DMLName Empty = new DMLName("List.Empty");

    // val null : 'a list -> bool
    final protected static class IsNull extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    val=val.request();
	    if (val instanceof DMLTuple) {
		DMLTuple t=(DMLTuple) val;
		if (t.getArity()!=1)
		    return DMLConstants.runtimeError.apply(null).raise();
		else {
		    DMLValue l = t.getByIndex(0);
		    if (l instanceof DMLList)
			return ((DMLList) l).isNull();
		    else
			return DMLConstants.runtimeError.apply(null).raise();
		}
	    }
	    else
		return DMLConstants.runtimeError.apply(null).raise();
	}
    }
    final public static IsNull isNull = new IsNull();
    // val length : 'a list -> int 
    // val @ : ('a list * 'a list) -> 'a list 
    // val hd : 'a list -> 'a 
    // val tl : 'a list -> 'a list 
    // val last : 'a list -> 'a 
    // val getItem : 'a list -> ('a * 'a list) option 
    // val nth : ('a list * int) -> 'a 
    // val take : ('a list * int) -> 'a list 
    // val drop : ('a list * int) -> 'a list 
    // val rev : 'a list -> 'a list 
    // val concat : 'a list list -> 'a list 
    // val revAppend : ('a list * 'a list) -> 'a list 
    // val app : ('a -> unit) -> 'a list -> unit 
    // val map : ('a -> 'b) -> 'a list -> 'b list 
    // val mapPartial : ('a -> 'b option) -> 'a list -> 'b list 
    // val find : ('a -> bool) -> 'a list -> 'a option 
    // val filter : ('a -> bool) -> 'a list -> 'a list 
    // val partition : ('a -> bool) -> 'a list -> ('a list * 'a list) 
    // val foldl : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b 
    // val foldr : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b 
    // val exists : ('a -> bool) -> 'a list -> bool 
    // val all : ('a -> bool) -> 'a list -> bool 
    // val tabulate : (int * (int -> 'a)) -> 'a list }
}
