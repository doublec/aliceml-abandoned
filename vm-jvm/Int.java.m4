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
final public class Int extends SCon {

    final public static Int MONE = new Int(-1);
    final public static Int ZERO = new Int(0);
    final public static Int ONE  = new Int(1);

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

    _BUILTIN(Uminus) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Int.~");
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument not Int",val);
	    }
	    return new
		Int(-((Int) v).getInt());
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
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).getInt();
	    int j = ((Int) w).getInt();
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
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).getInt();
	    int j = ((Int) w).getInt();
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
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).getInt();
	    int j = ((Int) w).getInt();
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
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).getInt();
	    int j = ((Int) w).getInt();
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
	    _fromTuple(args,val,1,"Int.abs");
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument not Int",val);
	    }
	    return new
		Int(java.lang.Math.abs(((Int) v).getInt()));
	}
    }
    /** <code>val abs : int -> int </code>*/
    _FIELD(Int,abs);

    _BUILTIN(Sign) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Int.sign");
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument not Int",val);
	    }
	    int i = ((Int) v).getInt();
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
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).getInt();
	    int j = ((Int) w).getInt();
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
	    _fromTuple(args,val,1,"Int.toString");
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		return _error("argument not Int",val);
	    }
	    int i = ((Int) v).getInt();
	    return new
		STRING (java.lang.String.valueOf(i));
	}
    }
    /** <code>val toString : int -> string </code>*/
    _FIELD(Int,toString);

    _BUILTIN(FromString) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Int.fromString");
	    DMLValue r = args[0].request();
	    if (!(r instanceof STRING)) {
		return _error("argument 1 not String",val);
	    }
	    try {
		java.lang.String sf = ((STRING) r).getString();
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
