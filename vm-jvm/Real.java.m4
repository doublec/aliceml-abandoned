package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLReal extends DMLSCon {

  /** Gleichheit der Real-Werte (Java-Floats) */
  final public boolean equals(Object val) {
    return (val instanceof DMLReal) && (((DMLReal)val).value==this.value);
  }

  public DMLReal(float value) {
    this.value=value;
  }

  float value=0.0f;

  final public String toString() {
    return value+": real";
  }

  final public float getFloat() {
    return value;
  }
}
