package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLRecordArity implements java.io.Serializable {

    DMLLabel[] labels = null;
    java.util.Hashtable hashtable=null;

    public DMLRecordArity(DMLLabel[] labels) {
	this(labels, new DMLValue[labels.length]);
    }

    public DMLRecordArity(DMLLabel[] labels, DMLValue[] vals) {
	super();
	int i=0;
	hashtable=new java.util.Hashtable();
	this.labels = labels;

	for(i=0; i<labels.length; i++) {
	    hashtable.put(labels[i], new Integer(i));
	}
    }

    final public int getIndexOfLabel(DMLLabel l) {
	Object idx=hashtable.get(l);
	if (idx==null)
	    return -1;
	else
	    return ((Integer) idx).intValue();
    }

    /** gibt den i-ten Label zurueck */
    final public DMLLabel getLabel(int i) {
	return labels[i];
    }

    final public boolean equals(Object o) {
	int i=0;
	if (!(o instanceof DMLRecordArity))
	    return false;
	DMLRecordArity other = (DMLRecordArity) o;
	if (labels.length != other.labels.length)
	    return false;
	for(i=0; i<labels.length; i++) {
	    if (!labels[i].equals(other.getLabel(i)))
		return false;
	}
	return true;
    }

    final public int hashCode() {
	int i=0;
	int hc=0;
	for(i=0; i<labels.length; i++)
	    hc=(hc+labels[i].hashCode()%0x3fffffff)%0x3fffffff;
	return hc;
    }
}
