/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert Real.
 *  @see Int
 *  @see SCon
 *  @see STRING 
 *  @see DMLValue
 *  @see Word
 */
final public class Real extends SCon {

    /** java-float Wert */
    private float value=0.0f;

    /** Baut einen neuen Real mit Inhalt <code>value</code>.
     *  @param value <code>float</code> Wert, der dem Real entspricht.
     */
    public Real(float value) {
	this.value=value;
    }

    /** Gleichheit der Real-Werte (Java-Floats) */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Real) && (((Real) val).value==this.value);
    }

    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return value+": real";
    }

    /** Den Java-Wert des Real auslesen.
     *  @return float Java-Wert der dem Real-Wert entspricht
     */
    final public float getFloat() {
	return value;
    }
    /** <code>structure Math : MATH</code>*/
    /** <code>val radix : int </code>*/
    /** <code>val precision : int </code>*/
    /** <code>val maxFinite : real </code>*/
    /** <code>val minPos : real </code>*/
    /** <code>val minNormalPos : real </code>*/
    /** <code>val posInf : real </code>*/
    /** <code>val negInf : real </code>*/
    _BUILTIN(Plus) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Real.+");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Real)) {
		_error("argument 1 not Real",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Real)) {
		_error("argument 2 not Real",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Real(((de.uni_sb.ps.dml.runtime.Real) v).getFloat() +
					      ((de.uni_sb.ps.dml.runtime.Real) v).getFloat());
	}
    }
    /** <code>val + : (real * real) -> real </code>*/
    _FIELD(Real,plus);

    _BUILTIN(Minus) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Real.-");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Real)) {
		_error("argument 1 not Real",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Real)) {
		_error("argument 2 not Real",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Real(((de.uni_sb.ps.dml.runtime.Real) v).getFloat() -
					      ((de.uni_sb.ps.dml.runtime.Real) v).getFloat());
	}
    }
    /** <code>val - : (real * real) -> real </code>*/
    _FIELD(Real,minus);


    _BUILTIN(Mult) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Real.*");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Real)) {
		_error("argument 1 not Real",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Real)) {
		_error("argument 2 not Real",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Real(((de.uni_sb.ps.dml.runtime.Real) v).getFloat() *
					      ((de.uni_sb.ps.dml.runtime.Real) v).getFloat());
	}
    }
    /** <code>val * : (real * real) -> real </code>*/
    _FIELD(Real,mult);

    _BUILTIN(Div) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Real./");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Real)) {
		_error("argument 1 not Real",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.Real)) {
		_error("argument 2 not Real",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.Real(((de.uni_sb.ps.dml.runtime.Real) v).getFloat() /
					      ((de.uni_sb.ps.dml.runtime.Real) v).getFloat());
	}
    }
    /** <code>val / : (real * real) -> real </code>*/
    _FIELD(Real,div);

    /** <code>val *+ : real * real * real -> real </code>*/
    /** <code>val *- : real * real * real -> real </code>*/
    /** <code>val ~ : real -> real </code>*/
    /** <code>val abs : real -> real </code>*/
    /** <code>val min : (real * real) -> real </code>*/
    /** <code>val max : (real * real) -> real </code>*/
    /** <code>val sign : real -> int </code>*/
    /** <code>val signBit : real -> bool </code>*/
    /** <code>val sameSign : (real * real) -> bool </code>*/
    /** <code>val copySign : (real * real) -> real </code>*/
    /** <code>val compare : (real * real) -> order </code>*/
    /** <code>val compareReal : (real * real) -> IEEEReal.real_order </code>*/
    /** <code>val < : (real * real) -> bool </code>*/
    /** <code>val <= : (real * real) -> bool </code>*/
    /** <code>val > : (real * real) -> bool </code>*/
    /** <code>val >= : (real * real) -> bool </code>*/
    /** <code>val == : (real * real) -> bool </code>*/
    /** <code>val != : (real * real) -> bool </code>*/
    /** <code>val ?= : (real * real) -> bool </code>*/
    /** <code>val unordered : (real * real) -> bool </code>*/
    /** <code>val isFinite : real -> bool </code>*/
    /** <code>val isNan : real -> bool </code>*/
    /** <code>val isNormal : real -> bool </code>*/
    /** <code>val class : real -> IEEEReal.float_class </code>*/
    /** <code>val fmt : java.lang.StringCvt.realfmt -> real -> string </code>*/
    /** <code>val toString : real -> string </code>*/
    _BUILTIN(FromString) {
	_APPLY(val) { 
	    _fromTuple(args,val,1,"Real.fromString");
	    _REQUESTDEC(DMLValue r,args[0]);
	    if (!(r instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    try {
		java.lang.String sf = ((STRING) r).getString();
		float f = Float.parseFloat(sf);
		return Option.SOME.apply(new de.uni_sb.ps.dml.runtime.Real(f));
	    } catch (NumberFormatException n) {
		return Option.NONE;
	    }
	}
    }
    /** <code>val fromString : string -> real option </code>*/
    _FIELD(Real,fromString);

    /** <code>val scan : (char, 'a) java.lang.StringCvt.reader -> (real, 'a) java.lang.StringCvt.reader </code>*/
    /** <code>val toManExp : real -> {man : real, exp : int} </code>*/
    /** <code>val fromManExp : {man : real, exp : int} -> real </code>*/
    /** <code>val split : real -> {whole : real, frac : real} </code>*/
    /** <code>val realMod : real -> real </code>*/
    /** <code>val rem : (real * real) -> real </code>*/
    /** <code>val nextAfter : (real * real) -> real </code>*/
    /** <code>val checkFloat : real ->real </code>*/
    /** <code>val realFloor : real -> real </code>*/
    /** <code>val realCeil : real -> real </code>*/
    /** <code>val realTrunc : real -> real </code>*/
    /** <code>val floor : real -> Int.int </code>*/
    /** <code>val ceil : real -> Int.int </code>*/
    /** <code>val trunc : real -> Int.int </code>*/
    /** <code>val round : real -> Int.int </code>*/
    /** <code>val toInt : IEEEReal.rounding_mode -> real -> int </code>*/
    /** <code>val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int </code>*/
    /** <code>val fromInt : int -> real </code>*/
    /** <code>val fromLargeInt : LargeInt.int -> real </code>*/
    /** <code>val toLarge : real -> LargeReal.real </code>*/
    /** <code>val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real </code>*/
    /** <code>val toDecimal : real -> IEEEReal.decimal_approx </code>*/
    /** <code>val fromDecimal : IEEEReal.decimal_approx -> real </code>*/
}
