package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLThread extends Thread implements DMLValue {

    DMLValue fcn=null;

    /** Gesamtzahl DMLThreads */
    static int totalNumber=0;

    /** Nummer des Threads */
    int threadNumber=0;

    public DMLThread(DMLValue v) {
	super();
	this.fcn=v;
	threadNumber=totalNumber++;
    }

    final public void run() {
	fcn.apply(DMLConstants.dmlunit);
    }

    final public String toString() {
	return "Thread["+threadNumber+"] ("+fcn+")\n"
	    +"Is a leif: "+this.isAlive()
	    +"Is interrupted: "+this.isInterrupted();
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue v) {
	return DMLConstants.runtimeError.apply(new DMLString("cannot apply "+this+" to "+v)).raise();
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }

    final private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
	DMLConstants.runtimeError.apply(new DMLString("cannot pickle DMLThread")).raise();
    }
}
