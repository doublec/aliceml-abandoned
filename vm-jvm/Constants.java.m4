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

import java.rmi.RemoteException;

final public class Constants {

    UNAME(dmlunit, unit);
    UNAME(dmltrue, true);
    UNAME(dmlfalse,false);
    UNAME(dmlmatch,[Match]);
    UNAME(dmlbind, [Bind]);

    UCONS(runtimeError,runtimeError);

    final public static class Ref extends UniqueConstructor {

	public Ref(java.lang.String n) {
	    super(n);
	}

	final public DMLValue apply(DMLValue val)
	    throws RemoteException {
	    return new Reference(val);
	}

	final public DMLValue apply0()
	    throws RemoteException {
	    _RAISENAME(General.Match);
	}
	final public DMLValue apply2(DMLValue v1, DMLValue v2)
	    throws RemoteException {
	    return new Reference(new Tuple2(v1,v2));
	}
	final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	    throws RemoteException {
	    return new Reference(new Tuple3(v1,v2,v3));
	}
	final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	    throws RemoteException {
	    return new Reference(new Tuple4(v1,v2,v3,v4));
	}

	final private Object readResolve()
	    throws java.io.ObjectStreamException {
	    Object o = GName.gNames.get(name);
	    // System.out.println("UC: "+o);
	    if (o==null) {
		GName.gNames.put(name,this);
		return this;
	    } else {
		return o;
	    }
	}
    }

    public static final Constructor reference;

    static {
	Object o = GName.gNames.get("reference");
	if (o== null) {
	    reference = new Ref("reference");
	} else {
	    reference = (Constructor) o;
	}
    }

    /** Diese Exception gibt's, um java-Exceptions zu verpacken */
    UCONS(javaAPIException,javaAPIException);

    /** Diese Exception zeigt wirkliche FEHLER des Benutzers an:
     *  - nicht-existierende Methoden/Konstructoren
     *  - Sicherheitsfehler
     */
    UCONS(javaAPIError,javaAPIError);

}
