package de.uni_sb.ps.DML.DMLRuntime;

abstract public class DMLFcnClosure extends DMLValue {

  public DMLFcnClosure() {
    super();
  }

  /** Gleicheit der FQ-Klassennamen */
  final public boolean equals(Object val) {
    return this.getClass().equals(val.getClass());
  }

  abstract public DMLValue apply(DMLValue val);

  final public String toString() {
    return "compiled function: "+this.getClass();
  }

}
