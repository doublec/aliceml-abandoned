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
import java.rmi.Remote;

public interface DMLPort extends Remote, DMLValue {

    public DMLValue apply(DMLValue val) throws RemoteException;
    public DMLValue apply0() throws java.rmi.RemoteException;
    public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws java.rmi.RemoteException;
    public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws java.rmi.RemoteException;
    public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws java.rmi.RemoteException;

    public DMLValue send(DMLValue msg) throws RemoteException;

    public DMLValue recieve() throws RemoteException;

    public boolean matchInt(int i)
	throws java.rmi.RemoteException;
    public boolean matchWord(int i)
	throws java.rmi.RemoteException;
    public boolean matchReal(float f)
	throws java.rmi.RemoteException;
    public boolean matchChar(char c)
	throws java.rmi.RemoteException;
    public boolean matchString(java.lang.String s)
	throws java.rmi.RemoteException;
}
