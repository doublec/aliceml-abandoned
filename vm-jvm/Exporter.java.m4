package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.*;

final class Exporter extends java.rmi.server.UnicastRemoteObject implements Export {

    java.util.Hashtable hash = null;

    public Exporter() throws java.rmi.RemoteException {}

    public Exporter(java.util.Hashtable h) throws java.rmi.RemoteException {
	hash=h;
    }

    public DMLValue get(java.lang.String what) throws java.rmi.RemoteException {
	return (DMLValue) hash.get(what);
    }
}
