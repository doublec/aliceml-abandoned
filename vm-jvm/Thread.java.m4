package de.uni_sb.ps.dml.runtime;
/** Diese Klasse repräsentiert DMLThreads.
 *  Threads sind First-Class; indem das Interface <code>DMLValue</code>
 *  implementiert wird, können sie wie andere Werte in DML verwendet werden.
 *  @see DMLValue
 */
public class DMLThread extends Thread implements DMLValue {
    /** Hier wird die Continuation für die Tail-Calls übergeben. */
    public DMLValue tail=null;

    /** Die Funktion (oder etwas, das zu einer Funktion wird),
     *  die der Thread ausführt.
     *  Die Funktion sollte den Typ fcn : unit -> 'a haben.
     */
    private DMLValue fcn=null;

    /** Gesamtzahl DMLThreads */
    public static int totalNumber=0;

    /** Nummer des Threads */
    private int threadNumber=0;

    public DMLThread() {
	super();
    }
    
    /** Erzeugt einen neuen DMLThread.
     *  @param v sollte eine Funktion f : unit-> 'a sein.
     */
    public DMLThread(DMLValue v) {
	this.fcn=v;
	threadNumber=totalNumber++;
    }

    /** Appliziert den DMLValue des Threads.
     *  Der Wert wird mit Argument unit appliziert. Der Rückgabewert wird
     *  verworfen.
     */
    public void run() {
	try {
	    fcn.apply(DMLConstants.dmlunit);
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	}
    }

    /** Stringdarstellung des DMLThread.
     *  Gibt die Nummer des Threads, den Status und die Gesamtzahl
     *  der bisher erzeugten Threads an.
     */
    final public String toString() {
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
	return DMLConstants.runtimeError.apply(new DMLString("cannot apply "+this+" to "+v)).raise();
    }

    /** Verpackt den Thread und wirft den ExceptionWrapper. */
    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }

    /** Die Methode verhindert, daß Threads gepickelt werden können.*/
    final private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
	DMLConstants.runtimeError.apply(new DMLString("cannot pickle DMLThread")).raise();
    }
}
