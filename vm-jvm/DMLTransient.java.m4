/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

import java.rmi.RemoteException;
import java.rmi.Remote;

public interface DMLLVar extends Remote, DMLValue {

    public DMLValue getValue() throws RemoteException;

    public DMLValue request() throws RemoteException;

    public DMLValue apply(DMLValue val) throws RemoteException;

    public DMLValue bind(DMLValue val) throws RemoteException;
}
