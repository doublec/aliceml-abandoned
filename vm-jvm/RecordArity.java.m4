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

final public class RecordArity implements java.io.Serializable {

    final protected Label[] labels;
    final protected java.util.Hashtable hashtable;

    public RecordArity(Label[] labels) {
	int l = labels.length;
	hashtable = new java.util.Hashtable(l); // hashtable with default size l
	this.labels = labels;

	for(int i=0; i<l; i++) {
	    hashtable.put(labels[i],new Integer(i)); // really?
	}
    }

    final public int getIndexOfLabel(Label l) {
	java.lang.Object idx=hashtable.get(l);
	if (idx==null)
	    return -1;
	else
	    return ((Integer) idx).intValue();
    }

    /** gibt den i-ten Label zurueck */
    final public Label getLabel(int i) {
	return labels[i];
    }

    final public boolean equals(java.lang.Object o) {
	if (!(o instanceof RecordArity)) {
	    return false;
	}
	RecordArity other = (RecordArity) o;
	if (labels.length != other.labels.length) {
	    return false;
	}
	int l = labels.length;
	for(int i=0; i<l; i++) {
	    if (!labels[i].equals(other.getLabel(i)))
		return false;
	}
	return true;
    }

    final public int hashCode() {
	int l=labels.length;
	int hc=0;
	for(int i=0; i<l; i++)
	    hc=(hc+labels[i].hashCode()%0x3fffffff)%0x3fffffff;
	return hc;
    }
}
