package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLRecordArity implements java.io.Serializable {
  DMLLabel[] labels=null;
  java.util.Hashtable hashtable=null;

  public DMLRecordArity(DMLLabel[] ls) {
    super();
    int i=0;
    labels=ls;
    hashtable=new java.util.Hashtable();

    for(i=0; i<labels.length; i++)
      hashtable.put(labels[i], new Integer(i));
  }

  /** gibt den Index des Labels l im Record zur"uck,
      RuntimeError falls l nicht vorhanden
  */
  final public int getByLabel(DMLLabel l) {
    Object idx=hashtable.get(l);
    if (idx==null)
      throw new DMLRuntimeError("label "+l+" not in record");
    else
      return ((Integer) idx).intValue();
  }

  /** gibt den i-ten Label als String zurueck */
  final public String getLabel(int i) {
    return labels[i].toString();
  }
}
