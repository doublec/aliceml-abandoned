package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLString extends DMLSCon {

  /** Testet Gleichheit der Java-Strings */
  final public boolean equals(DMLValue val) {
    return (val instanceof DMLString) &&
      (((DMLString)val).value.equals(this.value));
  }

  public DMLString(String value) {
    this.value=value;
  }

  final public String toString() {
    return "\""+value+"\": string";
  }

  String value=null;

  final public String getString() {
    return value;
  }
}
