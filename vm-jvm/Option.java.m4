/*
 * Author: 
 *      Daniel Simon, <dansim@ps.uni-sb.de>
 * 
 * Copyright:
 *      Daniel Simon, 1999
 *
 * Last change:
 *    $Date$ by $Author$
 * $Revision$
 * 
 */
package de.uni_sb.ps.dml.runtime;

final public class Option {
    // datatype 'a option = NONE | SOME of 'a 
    UNAME(NONE,Option.NONE);
    UCONS(SOME,Option.SOME);

    // exception Option
    UNAME(Option,Option.Option);

    // val getOpt : ('a option * 'a) -> 'a 
    // val isSome : 'a option -> bool 
    _BUILTIN(ValOf) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _REQUEST(val,val);
	    if (val instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) val;
		if (cv.getConstructor() == SOME) {
		    return cv.getContent();
		} else {
		    _RAISENAME(Option);
		}
	    } else {
		_error("argument not option",val);
	    }
	}
    }

    _FIELD(Option,valOf);
    // val valOf : 'a option -> 'a 
    // val filter : ('a -> bool) -> 'a -> 'a option 
    // val join : 'a option option -> 'a option 
    // val map : ('a -> 'b) -> 'a option -> 'b option 
    // val mapPartial : ('a -> 'b option) -> 'a option -> 'b option 
    // val compose : (('a -> 'b) * ('c -> 'a option)) -> 'c -> 'b option 
    // val composePartial : (('a -> 'b option) * ('c -> 'a option)) -> 'c -> 'b option
}
