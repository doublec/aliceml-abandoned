/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

public interface DMLConVal extends DMLValue {

    /** liefert den Inhalt */
    public DMLValue getContent() throws java.rmi.RemoteException;

    /** liefert den Constructor */
    public Constructor getConstructor() throws java.rmi.RemoteException;

    /** wird nur bei Ref funktionieren */
    public DMLValue assign(DMLValue val) throws java.rmi.RemoteException;

    public DMLValue get0() throws java.rmi.RemoteException;
    public DMLValue get1() throws java.rmi.RemoteException;
    public DMLValue get2() throws java.rmi.RemoteException;
    public DMLValue get3() throws java.rmi.RemoteException;
    public DMLValue get4() throws java.rmi.RemoteException;
}
