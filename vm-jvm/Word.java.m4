package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLWord extends DMLSCon {

    
  /** Gleichheit auf Integer-Werten */
  final public boolean equals(Object val) {
    return (val instanceof DMLWord) && (((DMLWord)val).value==this.value);
  }

  public DMLWord(int value) {
    this.value=value;
  }

  long value=0;

  final public String toString() {
    return value+": word";
  }

  final public long getInt() {
    return value;
  }
}
