package de.uni_sb.ps.DML.DMLRuntime;

public class DMLLVal implements DMLValue {

  public DMLLVal() {
    super();
    suspendVector = new java.util.Vector();
  }

  final public DMLValue getValue() { // gibt Wert zurück ohne blockieren
    if (ref==null)
      return this;
    else
      if (ref instanceof DMLLVal)
	return ref.getValue();
      else
	return ref;
  }

  final public DMLValue request() { // gibt Wert zurück wenn verfügbar
    if (ref==null) {
      Thread t=null;
      if (suspendVector==null)
	suspendVector=new java.util.Vector(2,10); // initial 2 Eintraege, Inkrement 10.
      suspendVector.addElement(t=Thread.currentThread());
      t.suspend();
    }
    if (ref instanceof DMLLVal) return ref.request();
    else return ref.getValue();
  }

  /** Gleichheit der referenzierten Werte, blockiert auf beiden Werten */
  final public boolean equals(Object val) {
    return (val instanceof DMLLVal) && this.request().equals(((DMLLVal) val).request());
  }

  protected DMLValue ref=null;

  protected java.util.Vector suspendVector=null;

  public DMLValue bind(DMLValue v) { // bindet Variable und startet Threads aus suspendVector-Liste
    ref=v;
    java.util.Enumeration e=suspendVector.elements();
    while (e.hasMoreElements())
      ((Thread) e.nextElement()).resume();
    return DMLConstants.dmlunit;
  }

  public String toString() {
    DMLValue val=this.getValue();
    if (val instanceof DMLLVal) return "<unresolved>: lval";
    return val.toString();
  }

  public DMLValue apply(DMLValue val) {
    throw new DMLRuntimeError("logic value cannot be applied.\n\t"+this+" applied to "+val);
  }

}
