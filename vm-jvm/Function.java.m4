package de.uni_sb.ps.DML.DMLRuntime;

abstract public class DMLFcnClosure implements DMLValue {

  public DMLFcnClosure() {
    super();
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

  /** Gleicheit der FQ-Klassennamen */
  final public boolean equals(DMLValue val) {
    return this.getClass().equals(val.getClass());
  }

  abstract public DMLValue apply(DMLValue val);

  final public String toString() {
    return "compiled function: "+this.getClass();
  }

}
