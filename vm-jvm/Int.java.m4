/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert Int.
 *  @see Real
 *  @see SCon
 *  @see STRING 
 *  @see DMLValue
 *  @see Word
 */
final public class Int implements DMLValue {

    final public static Int MONE = new Int(-1);
    final public static Int ZERO = new Int(0);
    final public static Int ONE  = new Int(1);

    /** java-int Wert */
    final public int value;

    /** Baut einen neuen Int mit Wert <code>value</code>.
     *  @param value <code>int</code> Wert, der dem Int entspricht.
     */
    public Int(int value) {
	this.value=value;
    }

    final public static boolean equals(Int i, Int j) {
	return i.value == j.value;
    }

    /** Gleichheit auf Integer-Werten */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Int) && (((Int) val).value==this.value);
    }

    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return value+": int";
    }

    /** Den Java-Wert des Int auslesen.
     *  @return int Java-Wert der dem Int-Wert entspricht
     */
    final public int getInt() {
	return value;
    }
    _apply_fails;

    _BUILTIN(Uminus) {
	_APPLY(val) {
	    _fromSingle(val,"Int.~");
	    if (!(val instanceof Int)) {
		_error("argument not Int",val);
	    }
	    return new
		Int(-((Int) val).value);
	}
    }
    /** <code>val ~ : int -> int </code>*/
    _FIELD(Int,uminus);
    _BINOPINT(mult,*);
    _BINOPINT(div,/);
    _BINOPINT(mod,%);

    /** <code>val quot : (int * int) -> int </code>*/
    final public static DMLValue quot = div;

    /** <code>val rem : (int * int) -> int </code>*/
    final public static DMLValue rem = mod;

    _BINOPINT(plus,+);
    _BINOPINT(minus,-);

    _BUILTIN(Compare) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.compare");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof Int)) {
		_error("argument 1 not Int",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(w instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).value;
	    int j = ((Int) w).value;
	    if (i==j) {
		return General.EQUAL;
	    } else if (i<j) {
		return General.LESS;
	    } else {
		return General.GREATER;
	    }
	}
    }
    /** <code>val compare : (int * int) -> order </code>*/
    _FIELD(Int,compare);

    _BUILTIN(Compare_) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.compare'");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof Int)) {
		_error("argument 1 not Int",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(w instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).value;
	    int j = ((Int) w).value;
	    if (i==j) {
		return ZERO;
	    } else if (i<j) {
		return MONE;
	    } else {
		return ONE;
	    }
	}
    }
    /** <code>val compare_ : (int * int) -> int </code>*/
    _FIELD(Int,compare_);

    _COMPAREINT(greater,>);
    _COMPAREINT(geq,>=);
    _COMPAREINT(less,<);
    _COMPAREINT(leq,<=);

    _BUILTIN(Min) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.min");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof Int)) {
		_error("argument 1 not Int",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(w instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).value;
	    int j = ((Int) w).value;
	    if (i<j) {
		return v;
	    } else {
		return w;
	    }
	}
    }
    /** <code>val min : (int * int) -> int </code>*/
    _FIELD(Int,min);

    _BUILTIN(Max) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.max");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof Int)) {
		_error("argument 1 not Int",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(w instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).value;
	    int j = ((Int) w).value;
	    if (i>j) {
		return v;
	    } else {
		return w;
	    }
	}
    }
    /** <code>val max : (int * int) -> int </code>*/
    _FIELD(Int,max);

    _BUILTIN(Abs) {
	_APPLY(val) {
	    _fromSingle(val,"Int.abs");
	    if (!(val instanceof Int)) {
		_error("argument not Int",val);
	    }
	    return new
		Int(java.lang.Math.abs(((Int) val).value));
	}
    }
    /** <code>val abs : int -> int </code>*/
    _FIELD(Int,abs);

    _BUILTIN(Sign) {
	_APPLY(val) {
	    _fromSingle(val,"Int.sign");
	    if (!(val instanceof Int)) {
		_error("argument not Int",val);
	    }
	    int i = ((Int) val).value;
	    if (i<0) {
		i=-1;
	    } else if (i>0) {
		i=1;
	    } // sonst ist i==0
	    return new
		Int(i);
	}
    }
    /** <code>val sign : int -> Int.int </code>*/
    _FIELD(Int,sign);

    _BUILTIN(SameSign) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.sameSign");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof Int)) {
		_error("argument 1 not Int",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(w instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).value;
	    int j = ((Int) w).value;
	    if ((i>0 && j>0) ||
		(i<0 && j<0) ||
		(i==j)) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val sameSign : (int * int) -> bool </code>*/
    _FIELD(Int,sameSign);

    _BUILTIN(ToString) {
	_APPLY(val) {
	    _fromSingle(val,"Int.toString");
	    if (!(val instanceof Int)) {
		_error("argument not Int",val);
	    }
	    int i = ((Int) val).value;
	    return new
		STRING (java.lang.String.valueOf(i));
	}
    }
    /** <code>val toString : int -> string </code>*/
    _FIELD(Int,toString);

    _BUILTIN(FromString) {
	_APPLY(val) {
	    _fromSingle(val,"Int.fromString");
	    if (!(val instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    try {
		java.lang.String sf = ((STRING) val).value;
		int f = Integer.parseInt(sf);
		return Option.SOME.apply(new Int(f));
	    } catch (NumberFormatException n) {
		return Option.NONE;
	    }
	}
    }
    /** <code>val fromString : string -> int option </code>*/
    _FIELD(Int,fromString);

    /** <code>val scan : java.lang.StringCvt.radix -> (char, 'a) java.lang.StringCvt.reader -> 'a -> (int * 'a) option </code>*/
    /** <code>val fmt : java.lang.StringCvt.radix -> int -> string </code>*/
    /** <code>val toLarge : int -> LargeInt.int </code>*/
    /** <code>val fromLarge : LargeInt.int -> int </code>*/
    /** <code>val toInt : int -> Int.int </code>*/
    /** <code>val fromInt : Int.int -> int </code>*/
    /** <code>val precision : Int.int option </code>*/
    /** <code>val minInt : int option </code>*/
    /** <code>val maxInt : int option </code>*/
}
