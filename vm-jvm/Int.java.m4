/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

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

    _BUILTIN(Uminus) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Int.~");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument not Int",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Int(-((de.uni_sb.ps.dml.runtime.Int) v).getInt());
	}
    }
    /** <code>val ~ : int -> int </code>*/
    _FIELD(uminus);

    _BUILTIN(Mult) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.*");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Int(((de.uni_sb.ps.dml.runtime.Int) v).getInt() *
					      ((de.uni_sb.ps.dml.runtime.Int) w).getInt());
	}
    }
    /** <code>val * : (int * int) -> int </code>*/
    _FIELD(mult);

    _BUILTIN(Div) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.div");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int scnd = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
	    if (scnd==0) {
		General.Div.raise();
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Int(((de.uni_sb.ps.dml.runtime.Int) v).getInt() /
					      scnd);
	}
    }
    /** <code>val div : (int * int) -> int </code>*/
    _FIELD(div);

    _BUILTIN(Mod) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.mod");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int scnd = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
	    if (scnd==0) {
		General.Div.raise();
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Int(((de.uni_sb.ps.dml.runtime.Int) v).getInt() %
					      scnd);
	}
    }
    /** <code>val mod : (int * int) -> int </code>*/
    _FIELD(mod);

    /** <code>val quot : (int * int) -> int </code>*/
    final public static Div quot = div;

    /** <code>val rem : (int * int) -> int </code>*/
    final public static Mod rem = mod;

    _BUILTIN(Plus) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.+");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Int(((de.uni_sb.ps.dml.runtime.Int) v).getInt() +
					      ((de.uni_sb.ps.dml.runtime.Int) w).getInt());
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
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Real(((de.uni_sb.ps.dml.runtime.Int) v).getInt() -
					      ((de.uni_sb.ps.dml.runtime.Int) w).getInt());
	}
    }
    /** <code>val - : (int * int) -> int </code>*/
    _FIELD(minus);

    _BUILTIN(Compare) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.compare");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    int j = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
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
    _FIELD(compare);

    _BUILTIN(Greater) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.>");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    int j = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
	    if (i>j) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val > : (int * int) -> bool </code>*/
    _FIELD(greater);

    _BUILTIN(Groreq) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.>=");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    int j = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
	    if (i>=j) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val >= : (int * int) -> bool </code>*/
    _FIELD(groreq);

    _BUILTIN(Less) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.<");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    int j = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
	    if (i<j) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val < : (int * int) -> bool </code>*/
    _FIELD(less);

    _BUILTIN(Leoreq) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.<=");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    int j = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
	    if (i<=j) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val <= : (int * int) -> bool </code>*/
    _FIELD(leoreq);

    _BUILTIN(Min) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.min");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    int j = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
	    if (i<j) {
		return v;
	    } else {
		return w;
	    }
	}
    }
    /** <code>val min : (int * int) -> int </code>*/
    _FIELD(min);

    _BUILTIN(Max) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.max");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    int j = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
	    if (i>j) {
		return v;
	    } else {
		return w;
	    }
	}
    }
    /** <code>val max : (int * int) -> int </code>*/
    _FIELD(max);

    _BUILTIN(Abs) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Int.abs");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument not Int",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Int(java.lang.Math.abs(((de.uni_sb.ps.dml.runtime.Int) v).getInt()));
	}
    }
    /** <code>val abs : int -> int </code>*/
    _FIELD(abs);

    _BUILTIN(Sign) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Int.sign");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    if (i<0) {
		i=-1;
	    } else if (i>0) {
		i=1;
	    } // sonst ist i==0
	    return new
		de.uni_sb.ps.dml.runtime.Int(i);
	}
    }
    /** <code>val sign : int -> Int.int </code>*/
    _FIELD(sign);

    _BUILTIN(SameSign) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.sameSign");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    int j = ((de.uni_sb.ps.dml.runtime.Int) w).getInt();
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
    _FIELD(sameSign);

    _BUILTIN(ToString) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Int.toString");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Int)) {
		return _error("argument not Int",val);
	    }
	    int i = ((de.uni_sb.ps.dml.runtime.Int) v).getInt();
	    return new
		de.uni_sb.ps.dml.runtime.String(i+"");
	}
    }
    /** <code>val toString : int -> string </code>*/
    _FIELD(toString);

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
    /** <code>val fmt : java.lang.StringCvt.radix -> int -> string </code>*/
    /** <code>val toLarge : int -> LargeInt.int </code>*/
    /** <code>val fromLarge : LargeInt.int -> int </code>*/
    /** <code>val toInt : int -> Int.int </code>*/
    /** <code>val fromInt : Int.int -> int </code>*/
    /** <code>val precision : Int.int option </code>*/
    /** <code>val minInt : int option </code>*/
    /** <code>val maxInt : int option </code>*/
}
