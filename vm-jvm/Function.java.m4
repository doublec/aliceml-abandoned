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

abstract public class Function implements DMLValue {

    static public boolean DEBUG = false;

    _BUILTIN(Debug) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    DEBUG=true;
	    return null;
	}
    }
    _FIELD(Function,debug);

    public Function() {
	super();
    }

    final public java.lang.String toString() {
	return "compiled function: "+this.getClass();
    }
}
