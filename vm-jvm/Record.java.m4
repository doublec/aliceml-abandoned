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

import java.util.Hashtable;

/** This class represents DML records. Records have an RecordArity and some
 *  associated values.
 */
final public class Record implements DMLTuple {

    static private Hashtable arityHash = new Hashtable();

    final public DMLValue vals[];
    final public RecordArity arity;

    public Record (java.lang.String[] ls, DMLValue[] vals) {
	this.vals = vals;
	RecordArity ra = new RecordArity(ls);
	Object ar=arityHash.get(ra);
	if (ar == null) {
	    arity = ra;
	    arityHash.put(arity,arity);
	} else {
	    arity = (RecordArity) ar;
	}
    }

    /** funktioniert nur, wenn records unique sind. */
    final public boolean equals(Object val) {
	if (val instanceof Record) {
	    Record r = (Record) val;
	    if (arity == r.arity) {
		for(int i=0; i<vals.length; i++) {
		    if (!vals[i].equals(r.vals[i])) {
			return false;
		    }
		}
		return true;
	    } else {
		return true;
	    }
	} else if (val instanceof DMLTransient) {
	    return val.equals(this);
	} else {
	    return false;
	}
    }

    final public java.lang.String toString() {
	java.lang.String s="{";
	int i;
	for (i=0; i<vals.length;i++) {
	    if (i>0) s+=", ";
	    s+=arity.getLabel(i)+" = "+vals[i];
	}
	return s+"}";
    }

    final public DMLValue get0() { return vals[0]; }
    final public DMLValue get1() { return vals[1]; }
    final public DMLValue get2() { return vals[2]; }
    final public DMLValue get3() { return vals[3]; }

    final public DMLValue get(int i) {
	int index = arity.getIndexOfLabel(java.lang.String.valueOf(i));
	if (index > -1)
	    return vals[index];
	else
	    return null;
    }

    final public DMLValue get(java.lang.String s) {
	int index = arity.getIndexOfLabel(s);
	if (index > -1)
	    return vals[index];
	else
	    return null;
    }

    final public DMLValue[] checkArity(java.lang.String[] lab) {
	RecordArity ar = new RecordArity(lab);
	Object cmp = arityHash.get(ar);
	if (cmp == arity) {
	    return vals;
	} else {
	    return null;
	}
    }

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return vals.length;
    }

    final public DMLValue[] getVals() {
	return vals;
    }
    _apply_fails ;
}
