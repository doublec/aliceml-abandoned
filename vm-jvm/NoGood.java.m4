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

/** The NoGood class is used to substitute Array and Threads in distribution.
 */
final public class NoGood implements DMLValue {

    final protected GName gName;

    public NoGood(GName gn) {
	gName = gn;
    }

    /** Falls unter diesm GName ein Objekt eingetragen ist, wird
     *  dieses verwendet, aus einem NoGood wird wieder ein Good.
     */
    final private java.lang.Object readResolve()
	throws java.io.ObjectStreamException {
	java.lang.Object o = GName.gNames.get(gName);
	if (o==null) {
	    return this;
	} else {
	    return o;
	}
    }

    _apply_fails ;
    _nomatch;
}

