package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLRecord implements DMLValue {

  public DMLRecord (DMLLabel[] ls, DMLValue[] vals) {
    this.vals=vals;
    arity=new RecordArity(ls);
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
  final public boolean equals(DMLValue val) {
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

  final public DMLValue getByLabel(DMLLabel label) {
    return vals[arity.getByLabel(label)];
  }

  final public DMLValue apply(DMLValue val) {
    throw new DMLRuntimeError("cannot apply record.\n\t"+this+" applied to "+val);
  }

  RecordArity arity=null;

  /** gibt den i-ten Eintrag des Records */
  final public DMLValue getByIndex(int i){
    return vals[i];
  }

  /** gibt die Stelligkeit des Records an */
  final public int getArity() {
    return vals.length;
  }
			     

}
