package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLFuture extends DMLLVal {

  public DMLFuture(DMLLVal lval) {
    super();
    ref=lval;
  }

  /** bind ist nicht erlaubt und wirft RuntimeError */
  final public DMLValue bind(DMLValue v) {
    throw new DMLCoEx1(DMLConstants.runtimeError, new DMLString("cannot bind future to "+v));
  }

  final public String toString() {
    DMLValue val=this.getValue();
    if (val instanceof DMLLVal) return "<unresolved>: future";
    return val.toString();
  }

  final public DMLValue apply(DMLValue val) {
    throw new DMLCoEx1(DMLConstants.runtimeError, new DMLString("future cannot be applied.\n\t"+this+" applied to "+val));
  }

}
