package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLRecord implements DMLValue {

    static private java.util.Hashtable arityHash = new java.util.Hashtable();

    public DMLRecord (DMLLabel[] ls, DMLValue[] vals) {
	this.vals=vals;
	arity=new DMLRecordArity(ls);
	if (! arityHash.contains(arity))
	    arityHash.put(arity,arity);
	else
	    arity = (DMLRecordArity) arityHash.get(arity);
    }

    final public int whatAreYou() {
	return DMLConstants.BUILTIN;
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    /** funktioniert nur, wenn records unique sind. */
    final public boolean equals(Object val) {
	return ((val instanceof DMLRecord) && ((DMLRecord) val).vals==this.vals);
    }

    DMLValue vals[]=null;

    final public DMLValue get(int index) {
	return vals[index];
    }

    final public String toString() {
	String s="{";
	int i;
	for (i=0; i<vals.length;i++) {
	    if (i>0) s+=", ";
	    s+=arity.getLabel(i)+" = "+vals[i];
	}
	return s+"): Record";
    }

    final public DMLValue getByLabel(String s) {
	int index = arity.getByLabel(new DMLLabel(s));
	if (index > -1)
	    return vals[index];
	else
	    return null;
    }
    
    final public DMLValue getByLabel(DMLLabel label) {
	int index = arity.getByLabel(label);
	if (index > -1)
	    return vals[index];
	else
	    return null;
    }

    final public DMLValue apply(DMLValue val) {
	throw new DMLRuntimeError("cannot apply record.\n\t"+this+" applied to "+val);
    }

    DMLRecordArity arity=null;

    /** gibt den i-ten Eintrag des Records */
    final public DMLValue getByIndex(int i){
	return vals[i];
    }

    /** gibt die Stelligkeit des Records an */
    final public int getArity() {
	return vals.length;
    }

    /** @parameter: arity,
	@returns: null, falls es diese Arity noch nicht gab,
	Zeiger auf die bereits dagewesene Arity sonst */
    final static public DMLRecordArity getRecordArity(DMLRecordArity arity) {
	return  (DMLRecordArity) arityHash.get(arity);
    }

    final public DMLRecordArity getRecordArity() {
	return arity;
    }
}
