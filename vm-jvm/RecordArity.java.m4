package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLRecordArity implements java.io.Serializable {
    DMLLabel[] labels = null;

    java.util.Hashtable hashtable=null;

    public DMLRecordArity(DMLLabel[] labels, DMLValue[] vals) {
	super();
	int i=0;
	hashtable=new java.util.Hashtable();

	qsort(labels,vals, 0,labels.length-1);
	for(i=0; i<labels.length; i++) {
	    hashtable.put(labels[i], new Integer(i));
	}
    }

    /** sortiert die Labels der Arity */
    private static void qsort(DMLLabel[] lab, DMLValue[] vals, int p, int r) {
	int q=0;
	if (p < r) {
	    q=partition(lab, vals,p,r);
	    qsort(lab,vals, p,q);
	    qsort(lab,vals, q+1,r);
	}
    }

    private static int partition(DMLLabel[] lab, DMLValue[] vals,int p, int r) {
	DMLLabel x = lab[p];
	DMLLabel dummy = null;
	DMLValue dumval = null;
	int i = p-1;
	int j = r+1;
	while (true) {
	    do j--; while (lab[j].compareTo(x) > 0);
	    do i++; while (lab[i].compareTo(x) < 0);
	    if (i<j) {
		dummy = lab[i];
		lab[i]=lab[j];
		lab[j]=dummy;
		dumval=vals[i];
		vals[i]=vals[j];
		vals[j]=dumval;
	    }
	    else
		return j;
	}
    }

    /** gibt den Index des Labels l im Record zur"uck,
	RuntimeError falls l nicht vorhanden
    */
    final public int getByLabel(DMLLabel l) {
	Object o = l.getName(),idx;
	if (o instanceof String)
	    idx=hashtable.get((String) o);
	else
	    idx=hashtable.get((Integer) o);
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
