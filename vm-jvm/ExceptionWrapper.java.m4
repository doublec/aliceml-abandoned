package de.uni_sb.ps.DML.DMLRuntime;

/** Wrapper, um DML-Werte als Exception werfen zu können.
 *  @see DMLValue#raise()
 */
final public class DMLExceptionWrapper extends RuntimeException {

    private DMLValue value = null;

    /** Baut einen neuen ExceptionWrapper.
     *  @param val der Wert der als Exception geworfen werden soll
     */
    public DMLExceptionWrapper(DMLValue val){
	value=val;
    }

    /** Liefert den Wert aus dem Wrapper.
     *  @return DMLValue Wert der als Exception geworfen wurde
     */
    final public DMLValue getValue() {
	return value;
    }

    /** Stringdarstellung des ExceptionWrappers.
     *  @return String Stringdarstellung des ExceptionWrappers
     */
    final public String toString() {
	return "\n\t"+value+" as exception";
    }
}
