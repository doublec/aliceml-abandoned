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

/** Diese Klasse repräsentiert Word.
 *  @see Int
 *  @see Real
 *  @see SCon
 *  @see STRING 
 *  @see DMLValue
 */
final public class Word implements DMLValue {

    /** java-long Wert */
    final public int value;

    /** Baut ein neues Word mit Wert <code>value</code>.
     *  @param value <code>long</code> Wert, der dem Word entspricht.
     */
    public Word(int value) {
	this.value=value;
    }

    final public static boolean equals(Word v, Word w) {
	return v.value == w.value;
    }

    /** Gleichheit auf Long-Werten */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Word) && (((Word)val).value==this.value);
    }

    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return value+": word";
    }

    _apply_fails;

    final public static int wordSizeI = 31;
    /** <code>val wordSize : int </code>*/
    final public static DMLValue wordSize = new Int(wordSizeI);

    /** <code>val toLargeWord : word -> LargeWord.word </code>*/
    /** <code>val toLargeWordX : word -> LargeWord.word </code>*/
    /** <code>val fromLargeWord : LargeWord.word -> word </code>*/
    /** <code>val toLargeInt : word -> LargeInt.int </code>*/
    /** <code>val toLargeIntX : word -> LargeInt.int </code>*/
    /** <code>val fromLargeInt : LargeInt.int -> word </code>*/

    _BUILTIN(ToInt) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    try {
		_REQUEST(val,val);
		return new Int(((Word) val).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
 	}
    }
    /** <code>val toInt : word -> Int.int </code>*/
    _FIELD(Word,toInt);

    _BUILTIN(ToIntX) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    try{
		_REQUEST(val,val);
		return new Int(((Word) val).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val toIntX : word -> Int.int </code>*/
    _FIELD(Word,toIntX);

    _BUILTIN(FromIntStrich) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // möglicherweise nicht so ganz richtig
	    _fromTuple(args,val,2,"Word.fromInt'");
	}
	_SAPPLY2(v) {
	    try {
		_REQUEST(v1,v1);
		int bits = ((Int) v1).value;
		if (bits > 32) {
		    _error("argument to large",v1);
		} else {
		    _REQUEST(v2,v2);
		    return new Word(((Int) v2).value);
		}
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val fromInt' : (int * int) -> word </code>*/
    final public static DMLValue fromIntStrich = new FromIntStrich();
    static {
	Builtin.builtins.put("Word.fromInt'",fromIntStrich);
    }

    /** <code>val fromInt : Int.int -> word </code>*/

    _BUILTIN(Orb) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
		_fromTuple(args,val,2,"Word.orb");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue x,v1);
		_REQUESTDEC(DMLValue y,v2);
		return new Word(((Word) x).value | ((Word) y).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val orb : (word * word) -> word </code>*/
    _FIELD(Word,orb);

    _BUILTIN(Xorb) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
		_fromTuple(args,val,2,"Word.xorb");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue x,v1);
		_REQUESTDEC(DMLValue y,v2);
		return new Word(((Word) x).value ^ ((Word) y).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val xorb : (word * word) -> word </code>*/
    _FIELD(Word,xorb);

    _BUILTIN(Andb) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
		_fromTuple(args,val,2,"Word.andb");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue x,v1);
		_REQUESTDEC(DMLValue y,v2);
		return new Word(((Word) x).value & ((Word) y).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val andb : (word * word) -> word </code>*/
    _FIELD(Word,andb);

    _BUILTIN(Notb) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    try {
		_REQUESTDEC(DMLValue y,val);
		return new Word(((Word) y).value ^ 0x7fffffff);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val notb : word -> word </code>*/
    _FIELD(Word,notb);

    _BUILTIN(Left) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
		_fromTuple(args,val,2,"Word.<<");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue x,v1);
		_REQUESTDEC(DMLValue y,v2);
		return new Word(((Word) x).value << ((Word) y).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val << : (word * Word.word) -> word </code>*/
    final public static DMLValue left = new Left();
    static {
	Builtin.builtins.put("Word.<<",left);
    }

    _BUILTIN(Right) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
		_fromTuple(args,val,2,"Word.>>");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue x,v1);
		_REQUESTDEC(DMLValue y,v2);
		return new Word(((Word) x).value >> ((Word) y).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val >> : (word * Word.word) -> word </code>*/
    final public static DMLValue right = new Right();
    static {
	Builtin.builtins.put("Word.>>",right);
    }

    _BUILTIN(NRight) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
		_fromTuple(args,val,2,"Word.>>");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue x,v1);
		_REQUESTDEC(DMLValue y,v2);
		return new Word(((Word) x).value >>> ((Word) y).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val ~>> : (word * Word.word) -> word </code>*/
    final public static DMLValue nRight = new NRight();
    static {
	Builtin.builtins.put("Word.~>>",nRight);
    }
    /** <code>val + : (word * word) -> word </code>*/
    /** <code>val - : (word * word) -> word </code>*/
    /** <code>val * : (word * word) -> word </code>*/
    /** <code>val div : (word * word) -> word </code>*/
    /** <code>val mod : (word * word) -> word </code>*/
    /** <code>val compare : (word * word) -> order </code>*/
    /** <code>val > : (word * word) -> bool </code>*/
    /** <code>val < : (word * word) -> bool </code>*/
    /** <code>val >= : (word * word) -> bool </code>*/
    /** <code>val <= : (word * word) -> bool </code>*/
    /** <code>val min : (word * word) -> word </code>*/
    /** <code>val max : (word * word) -> word </code>*/
    /** <code>val fmt : java.lang.StringCvt.radix -> word -> string </code>*/

    _BUILTIN(ToString) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    try {
		_REQUESTDEC(DMLValue y,val);
		return new STRING (java.lang.String.valueOf(((Word) y).value));
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val toString : word -> string </code>*/
    _FIELD(Word,toString);
    /** <code>val fromString : string -> word option </code>*/
    /** <code>val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> 'a -> (word, 'a) option</code>*/
    public final boolean matchInt(int i)
	throws java.rmi.RemoteException { return false; }
    public final boolean matchWord(int i)
	throws java.rmi.RemoteException { return (value==i); }
    public final boolean matchReal(float f)
	throws java.rmi.RemoteException { return false; }
    public final boolean matchChar(char c)
	throws java.rmi.RemoteException { return false; }
    public final boolean matchString(java.lang.String s)
	throws java.rmi.RemoteException { return false; }
}
