/*
 * Author: 
 *      Daniel Simon, <dansim@ps.uni-sb.de>
 * 
 * Copyright:
 *      Daniel Simon, 1999
 *
 * Last change:
 *    $Date$ by $Author$
 * $Revision$
 * 
 */
package de.uni_sb.ps.dml.runtime;

/** Wrapper, um DML-Werte als Exception werfen zu können.
 */
final public class ExceptionWrapper extends RuntimeException {

    final public DMLValue value;

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
	try {
	    return "\n\t"+value.toString(3)+" as exception";
	} catch (java.rmi.RemoteException r) {
	    return null;
	}
    }
}
