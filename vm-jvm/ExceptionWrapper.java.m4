/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Wrapper, um DML-Werte als Exception werfen zu können.
 *  @see DMLValue#raise()
 */
final public class ExceptionWrapper extends RuntimeException {

    private DMLValue value = null;

    /** Baut einen neuen ExceptionWrapper.
     *  @param val der Wert der als Exception geworfen werden soll
     */
    public ExceptionWrapper(DMLValue val){
	value=val;
    }

    /** Liefert den Wert aus dem Wrapper.
     *  @return DMLValue Wert der als Exception geworfen wurde
     */
    final public DMLValue getValue() {
	return value;
    }

    /** java.lang.Stringdarstellung des ExceptionWrappers.
     *  @return java.lang.String java.lang.Stringdarstellung des ExceptionWrappers
     */
    final public java.lang.String toString() {
	return "\n\t"+value+" as exception";
    }
}
