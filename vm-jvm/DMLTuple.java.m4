package de.uni_sb.ps.DML.DMLRuntime;

public class DMLTuple implements DMLValue {

    
    DMLValue vals[]=null;

    public DMLTuple(DMLValue[] vals) {
	super();
	this.vals=vals;
    }

    public boolean equals(Object val) {
	DMLTuple r=null;
	int i=0;
	if (!(val instanceof DMLTuple) ||
	     (val instanceof DMLRecord))
	    return false;
	else {
	    r = (DMLTuple) val;
	    if (r.vals.length != this.vals.length)
		return false;
	    for(i=0; i<vals.length; i++)
		if (!vals[i].equals(r.vals[i])) return false;
	    return true;
	}
    }

    public String toString() {
	String s="(";
	int i;
	for (i=0; i<vals.length;i++) {
	    if (i>0) s+=", ";
	    s+=vals[i];
	}
	return s+")";
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue getByIndex(int i){
	return vals[i];
    }

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return vals.length;
    }

    final public DMLValue[] getVals() {
	return vals;
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue v) {
	return DMLConstants.runtimeError.apply(new DMLString("cannot apply "+this+" to "+v)).raise();
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }
}
