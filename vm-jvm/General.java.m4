package de.uni_sb.ps.DML.DMLBuiltin;

import de.uni_sb.ps.DML.DMLRuntime.*;

final public class General {
    /** <code>datatype order = LESS | EQUAL | GREATER</code>*/
    final public static DMLName LESS = new DMLName("order.LESS");
    final public static DMLName EQUAL = new DMLName("order.EQUAL");
    final public static DMLName GREATER = new DMLName("order.GREATER");
    /** <code>exception Bind</code>*/
    final public static DMLName Bind = new DMLName("General.Bind");
    /** <code>exception Chr</code>*/
    final public static DMLName Chr = new DMLName("General.Chr");
    /** <code>exception Div</code>*/
    final public static DMLName Div = new DMLName("General.Div");
    /** <code>exception Domain</code>*/
    final public static DMLName Domain = new DMLName("General.Domain");
    /** <code>exception Fail of string </code>*/
    final public static DMLConstructor Fail = new DMLConstructor("General.Fail");
    /** <code>exception Match</code>*/
    final public static DMLName Match = new DMLName("General.Match");
    /** <code>exception Overflow</code>*/
    final public static DMLName Overflow = new DMLName("General.Overflow");
    /** <code>exception Size</code>*/
    final public static DMLName Size = new DMLName("General.Size");
    /** <code>exception Span</code>*/
    final public static DMLName Span = new DMLName("General.Span");
    /** <code>exception Subscript</code>*/
    final public static DMLName Subscript = new DMLName("General.Subscript");

    final protected static class Deref extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args = fromTuple(val,1,"deref");
	    DMLValue arg = args[0].request();
	    if (arg instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) arg;
		if (cv.getConstructor()==DMLConstants.reference)
		    return cv.getContent();
	    }
	    return error("wrong argument #1 for deref",val);
	}
    }
    /** <code>val ! : 'a ref -> 'a</code>*/
    final public static Deref deref  = new Deref();

    final protected static class Assign extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args = fromTuple(val,2,"assign");
	    DMLValue car=args[0].request();
	    if (car instanceof DMLConVal) {
		return ((DMLConVal) car).assign(args[1]);
	    }
	    else
		return error("wrong argument #1 for assign",val);
	}
    }
    /** <code>val := : ('a ref * 'a) -> unit</code>*/
    final public static Assign assign = new Assign();

    final protected static class Compose extends DMLBuiltin {
	final protected class Composer extends DMLBuiltin {
	    protected DMLValue f,g;
	    public Composer(DMLValue f, DMLValue g) {
		this.f=f;
		this.g=g;
	    }
	    final public DMLValue apply(DMLValue v) {
		return f.apply(g.apply(v));
	    }
	}
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args = fromTuple(val,2,"assign");
	    DMLValue f = args[0].request();
	    DMLValue g = args[1].request();
	    return new Composer(f,g);
	}
    }
    /** <code>val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c </code>*/
    final public static Compose o = new Compose();

    final protected static class Before extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args  = fromTuple(val,2,"before");
	    return args[0];
	}
    }
    /** <code>val before : ('a * unit) -> 'a</code>*/
    // wirft einfach das zweite Argument weg
    final public static Before before = new Before();

    final protected static class Ignore extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    return DMLConstants.dmlunit;
	}
    }
    /** <code>val ignore : 'a -> unit </code>*/
    final public static Ignore ignore = new Ignore();

    final protected static class LVar extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    return new DMLLVar();
	}
    }
    /** <code>val lvar : _ -> lvar</code>*/
    final public static LVar lvar = new LVar();

    /** Ref-Zellen-Konstruktor, entspricht etwa NewCell oder so.*/
    final protected static class Ref extends DMLBuiltin {
	final synchronized public DMLValue apply(DMLValue val) {
	    // --> Tuple?
	    return new DMLConVal(DMLConstants.reference,val);
	}
    }
    /** <code>val ref : 'a -> ref 'a</code>*/
    final public static Ref ref = new Ref();

    final protected static class Spawn extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"spawn");
	    DMLThread t=new DMLThread(args[0]);
	    t.start();
	    return DMLConstants.dmlunit;
	}
    }
    /** <code>val spawn : (_ -> 'a) -> unit</code>
     *  spawn startet einen neuen DMLThread, der das Argument
     *  von <code>apply</code> appliziert.
     */
    final public static Spawn spawn = new Spawn();

    final protected static class Equals extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,2,"equals");
	    DMLValue car=args[0].request();
	    DMLValue cdr=args[1].request();
	    if (car.equals(cdr))
		return DMLConstants.dmltrue;
	    else
		return DMLConstants.dmlfalse;
	}
    }
    /** <code>val equals : ('a * 'b) -> bool</code>*/
    final public static Equals equals = new Equals();

    // val exnName : exn -> string 
    // val exnMessage : exn -> string

    // Hilfsfunktionen
    final protected static DMLValue[] fromTuple
	(DMLValue v, // Value-Tuple
	 int ea,     // erwartete Anzahl Argumente
	 String errMsg) {
	v=v.request();
	if (v instanceof DMLTuple) {
	    DMLTuple t=(DMLTuple) v;
	    if (t.getArity()==ea) {
		DMLValue[] vals = new DMLValue[ea];
		for(int i=0; i<ea; i++)
		    vals[i]=t.getByIndex(i);
		return vals;
	    }
	    else
		error("wrong number of arguments in "+errMsg, v);
	}
	else
	    error("wrong argument type for "+errMsg,v);
	return null;
    }

    final protected static DMLValue error
	(String msg, DMLValue v) {
	// sonst: Fehler
	DMLValue[] err = {
	    new DMLString(msg),
	    v};
	return DMLConstants.
	    runtimeError.apply(new DMLTuple(err)).raise();
    }
}
