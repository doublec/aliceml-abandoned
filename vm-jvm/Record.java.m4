/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** This class represents DML records. Records have an RecordArity and some
 *  associated values.
 */
final public class Record extends Tuple {

    static private java.util.Hashtable arityHash = new java.util.Hashtable();

    RecordArity arity=null;

    public Record (Label[] ls, DMLValue[] vals) {
	super(null);
	java.lang.Object ar=null;
	this.vals=vals;
	arity=new RecordArity(ls);
	ar=arityHash.get(arity);
	if (ar==null)
	    arityHash.put(arity,arity);
	else
	    arity =(RecordArity) ar;
    }

    /** funktioniert nur, wenn records unique sind. */
    final public boolean equals(java.lang.Object val) {
	Record r=null;
	int i=0;
	if (!(val instanceof Record))
	    return false;
	else {
	    r = (Record) val;
	    if (!r.getRecordArity().equals(this.arity))
		return false;
	    for(i=0; i<vals.length; i++)
		if (!vals[i].equals(r.vals[i])) return false;
	    return true;
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

    final public DMLValue get(int i) {
	int index = arity.getIndexOfLabel(new Label(i));
	if (index > -1)
	    return vals[index];
	else
	    return null;
    }

    final public DMLValue get(java.lang.String s) {
	int index = arity.getIndexOfLabel(new Label(s));
	if (index > -1)
	    return vals[index];
	else
	    return null;
    }

    final public DMLValue get(Label label) {
	int index = arity.getIndexOfLabel(label);
	if (index > -1)
	    return vals[index];
	else
	    return null;
    }


    /** @parameter: arity,
	@returns: null, falls es diese Arity noch nicht gab,
	Zeiger auf die bereits dagewesene Arity sonst */
    final static public RecordArity getRecordArity(RecordArity arity) {
	return  (RecordArity) arityHash.get(arity);
    }

    final public RecordArity getRecordArity() {
	return arity;
    }
}
