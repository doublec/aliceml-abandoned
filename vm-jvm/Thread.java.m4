package de.uni_sb.ps.dml.runtime;

import de.uni_sb.ps.dml.builtin.NoGood;
/** Diese Klasse repräsentiert de.uni_sb.ps.dml.runtime.Threads.
 *  java.lang.Threads sind First-Class; indem das Interface <code>DMLValue</code>
 *  implementiert wird, können sie wie andere Werte in DML verwendet werden.
 *  @see DMLValue
 */
public class Thread extends java.lang.Thread implements DMLValue {
    /** Hier wird die Continuation für die Tail-Calls übergeben. */
    public DMLValue tail=null;
    private NoGood ng = null;
    /** Die Funktion (oder etwas, das zu einer Funktion wird),
     *  die der java.lang.Thread ausführt.
     *  Die Funktion sollte den Typ fcn : unit -> 'a haben.
     */
    private DMLValue fcn=null;

    /** Gesamtzahl de.uni_sb.ps.dml.runtime.Threads */
    public static int totalNumber=0;

    /** Nummer des java.lang.Threads */
    private int threadNumber=0;

    public Thread() {
    }

    /** Erzeugt einen neuen de.uni_sb.ps.dml.runtime.Thread.
     *  @param v sollte eine Funktion f : unit-> 'a sein.
     */
    public Thread(DMLValue v) {
	this.fcn=v;
	threadNumber=totalNumber++;
    }

    /** Appliziert den DMLValue des java.lang.Threads.
     *  Der Wert wird mit Argument unit appliziert. Der Rückgabewert wird
     *  verworfen.
     */
    public void run() {
	try {
	    fcn.apply(Constants.dmlunit);
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	}
    }

    /** java.lang.Stringdarstellung des de.uni_sb.ps.dml.runtime.Thread.
     *  Gibt die Nummer des java.lang.Threads, den Status und die Gesamtzahl
     *  der bisher erzeugten java.lang.Threads an.
     */
    final public java.lang.String toString() {
	return "Thread["+threadNumber+"] ("+fcn+")\n"
	    +"Is a leif: "+this.isAlive()
	    +"Is interrupted: "+this.isInterrupted();
    }

    /** Liefert sich selbst. */
    final public DMLValue getValue() {
	return this;
    }

    /** Liefert sich selbst. */
    final public DMLValue request() {
	return this;
    }

    /** Erzeugt Laufzeitfehler.
     *  @param val wird nicht requested
     *  @return DMLValue es wird immer eine Exception geworfen.
     */
    final public DMLValue apply(DMLValue v) throws java.rmi.RemoteException {
	return Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot apply "+this+" to "+v)).raise();
    }

    /** Verpackt den java.lang.Thread und wirft den ExceptionWrapper. */
    final public DMLValue raise() {
	throw new ExceptionWrapper(this);
    }

    private Object writeReplace()
	throws java.io.ObjectStreamException {
	if (ng==null) { // falls zum ersten Mal serialisiert
	    GName gn=new GName();
	    ng=new NoGood(gn);
	    GName.gNames.put(gn,ng);
	    return ng;
	} else {
	    return ng;
	}
    }
}
