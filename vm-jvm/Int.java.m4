/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

import  de.uni_sb.ps.dml.builtin.Option;

/** Diese Klasse repräsentiert Int.
 *  @see Real
 *  @see SCon
 *  @see de.uni_sb.ps.dml.runtime.String
 *  @see DMLValue
 *  @see Word
 */
final public class Int extends SCon {
    /** java-int Wert */
    private int value=0;

    /** Baut einen neuen Int mit Wert <code>value</code>.
     *  @param value <code>int</code> Wert, der dem Int entspricht.
     */
    public Int(int value) {
	this.value=value;
    }

    /** Gleichheit auf Integer-Werten */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Int) && (((Int) val).value==this.value);
    }

    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return value+": int or char";
    }

    /** Den Java-Wert des Int auslesen.
     *  @return int Java-Wert der dem Int-Wert entspricht
     */
    final public int getInt() {
	return value;
    }


    /** <code>val toLarge : int -> LargeInt.int </code>*/
    /** <code>val fromLarge : LargeInt.int -> int </code>*/
    /** <code>val toInt : int -> Int.int </code>*/
    /** <code>val fromInt : Int.int -> int </code>*/
    /** <code>val precision : Int.int option </code>*/
    /** <code>val minInt : int option </code>*/
    /** <code>val maxInt : int option </code>*/
    /** <code>val ~ : int -> int </code>*/
    /** <code>val * : (int * int) -> int </code>*/
    /** <code>val div : (int * int) -> int </code>*/
    /** <code>val mod : (int * int) -> int </code>*/
    /** <code>val quot : (int * int) -> int </code>*/
    /** <code>val rem : (int * int) -> int </code>*/
    _BUILTIN(Plus) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.+");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Real(((de.uni_sb.ps.dml.runtime.Int) v).getInt() +
					      ((de.uni_sb.ps.dml.runtime.Int) v).getInt());
	}
    }
    /** <code>val + : (int * int) -> int </code>*/
    _FIELD(plus);

    _BUILTIN(Minus) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.-");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Real(((de.uni_sb.ps.dml.runtime.Int) v).getInt() -
					      ((de.uni_sb.ps.dml.runtime.Int) v).getInt());
	}
    }
    /** <code>val - : (int * int) -> int </code>*/
    _FIELD(minus);

    /** <code>val compare : (int * int) -> order </code>*/
    /** <code>val > : (int * int) -> bool </code>*/
    /** <code>val >= : (int * int) -> bool </code>*/
    /** <code>val < : (int * int) -> bool </code>*/
    /** <code>val <= : (int * int) -> bool </code>*/
    /** <code>val abs : int -> int </code>*/
    /** <code>val min : (int * int) -> int </code>*/
    /** <code>val max : (int * int) -> int </code>*/
    /** <code>val sign : int -> Int.int </code>*/
    /** <code>val sameSign : (int * int) -> bool </code>*/
    /** <code>val fmt : java.lang.StringCvt.radix -> int -> string </code>*/
    /** <code>val toString : int -> string </code>*/
    _BUILTIN(FromString) {
	_APPLY(val) { 
	    _fromTuple(args,val,1,"Int.fromString");
	    DMLValue r = args[0].request();
	    if (!(r instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    try {
		java.lang.String sf = ((de.uni_sb.ps.dml.runtime.String) r).getString();
		int f = Integer.parseInt(sf);
		return Option.SOME.apply(new de.uni_sb.ps.dml.runtime.Int(f));
	    } catch (NumberFormatException n) {
		return Option.NONE;
	    }
	}
    }
    /** <code>val fromString : string -> int option </code>*/
    _FIELD(fromString);

    /** <code>val scan : java.lang.StringCvt.radix -> (char, 'a) java.lang.StringCvt.reader -> 'a -> (int * 'a) option </code>*/
}
