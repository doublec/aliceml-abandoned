package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLInt extends DMLSCon {

  /** Gleichheit auf Integer-Werten */
  final public boolean equals(Object val) {
    return (val instanceof DMLInt) && (((DMLInt)val).value==this.value);
  }

  public DMLInt(int value) {
    this.value=value;
  }

  int value=0;

  final public String toString() {
    return value+": int, word or char";
  }

  final public int getInt() {
    return value;
  }

}
