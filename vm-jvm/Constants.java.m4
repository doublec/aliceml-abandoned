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

final public class Constants {

    public static final Name dmlunit = new UniqueName("unit");
    public static final Name dmltrue = new UniqueName("true");
    public static final Name dmlfalse= new UniqueName("false");
    //    public static final UniqueName dmlnil  = new UniqueName("nil"); -> siehe List

    public static final Name dmlmatch = new UniqueName("[Match]");
    public static final Name dmlbind  = new UniqueName("[Bind]");

    public static final Constructor runtimeError = new UniqueConstructor("runtimeError");

    final public static class Ref extends UniqueConstructor {

	public Ref(java.lang.String n) {
	    super(n);
	}

	final public DMLValue apply(DMLValue val)
	    throws java.rmi.RemoteException {
	    return new Reference(val);
	}
    }

    public static final Constructor reference    = new Ref("reference");
    //    public static final Constructor cons         = new Constructor("cons");

    /** Diese Exception gibt's, um java-Exceptions zu verpacken */
    public static final Constructor javaAPIException = new UniqueConstructor("javaAPIException");

    /** Diese Exception zeigt wirkliche FEHLER des Benutzers an:
     *  - nicht-existierende Methoden/Konstructoren
     *  - Sicherheitsfehler
     */
    public static final Constructor javaAPIError = new UniqueConstructor("javaAPIError");

}
