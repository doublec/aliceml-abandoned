/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

final public class JObject implements DMLValue {

    java.lang.Object javaObject = null;

    public JObject(java.lang.Object o) {
	super();
	javaObject=o;
    }

    final public java.lang.Object getObject() {
	return javaObject;
    }

    final public java.lang.String toString() {
	return javaObject.getClass().getName()+": "+javaObject+" : APIObject";
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
    /** Builtin zur Benutzung von java API Objekt/Klassen-Methoden
 *  @author Daniel
 */
    final protected static class InvokeMethod extends Builtin {
	/** Methode entspricht der Applikation einer Methode
	 *  invoke: string/JObject * string * arg0 * ... * argN -> JObject
	 *  @param v v sollte ein Tupel sein mit
	 *  @param #1 de.uni_sb.ps.dml.runtime.String als Klassenname oder
	 *  @param #1 JObject mit einem java.lang.Object der Klasse
	 *  @param args Argumente für die Methode, kann leer sein
	 */
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    val = val.request();
	    if (val instanceof DMLTuple) {
		DMLTuple v = (DMLTuple) val;
		DMLValue c = v.getByIndex(0).request(); // erstes Argument ist java.lang.Object oder Klasse

		Class cl = null;
		Object object = null;
		java.lang.String classname = null;
		if (c instanceof de.uni_sb.ps.dml.runtime.String) { // nur noch Klasse
		    classname = ((de.uni_sb.ps.dml.runtime.String) c).getString();
		    try {
			cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		    } catch (ClassNotFoundException e) {
			DMLValue[] err = {
			    new de.uni_sb.ps.dml.runtime.String("cannot find class "+classname),
			    val};
			return Constants.javaAPIError.apply(new Tuple(err)).raise();
		    }
		} else if (c instanceof JObject) {
		    object = ((JObject) c).getObject();
		    cl = object.getClass();
		    classname=cl.getName();
		} else { // Fehler
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String("illegal argument for classtype"),
			val};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
		// Klasse gefunden, Class steht in cl

		DMLValue mn = v.getByIndex(1).request(); // Methodenname
		java.lang.String methname = null;
		if (!(mn instanceof de.uni_sb.ps.dml.runtime.String)) {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String("illegal argument for methodname"),
			val};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
		else
		    methname = ((de.uni_sb.ps.dml.runtime.String) mn).getString();
		// System.out.println("Object: "+classname+" Method: "+methname);
		int length = v.getArity()-2; // Anzahl der Argumente für Methode
		Class[] classes = null;
		Object[] args   = null;
		java.lang.reflect.Method meth = null;
		if (length==0 || v.getByIndex(2).request()==Constants.dmlunit) { // nullstellige Methode
		    try {
			meth = cl.getMethod(methname,null);
		    } catch (NoSuchMethodException e) {
			DMLValue[] err = {
			    new de.uni_sb.ps.dml.runtime.String("no nullary method available for "+classname),
			    val};
			return Constants.javaAPIError.apply(new Tuple(err)).raise();
		    }
		}
		else { // Methode hat >= 1 Argumente
		    classes = new Class[length];
		    args = new java.lang.Object[length];
		    for(int i=0; i<length; i++) {
			DMLValue helper = v.getByIndex(i+2).request();
			if (helper instanceof SCon) { // Argument ist de.uni_sb.ps.dml.runtime.String, Int, Real oder Word
			    if (helper instanceof Int) {
				classes[i] = java.lang.Integer.TYPE;
				args[i] = new java.lang.Integer(((Int) helper).getInt());
			    }
			    else if (helper instanceof de.uni_sb.ps.dml.runtime.Real) {
				classes[i] = Float.TYPE;
				args[i] = new Float(((de.uni_sb.ps.dml.runtime.Real) helper).getFloat());
			    }
			    else if (helper instanceof de.uni_sb.ps.dml.runtime.String) {
				try{
				    classes[i] = Class.forName("java.lang.String");  // java.lang.String.TYPE;
				} catch (ClassNotFoundException e) {
				// This should NEVER happen
				    System.err.println("internal runtime error: new_instance");
				    System.err.println(e.getMessage());
				    e.printStackTrace();
				}
				args[i] = ((de.uni_sb.ps.dml.runtime.String) helper).getString();
			    }
			    else if (helper instanceof de.uni_sb.ps.dml.runtime.Word) {
				classes[i] = Long.TYPE;
				args[i] = new Long(((de.uni_sb.ps.dml.runtime.Word) helper).getLong());
			    }
			}
			else if (helper instanceof Name) {
			    if (helper==Constants.dmltrue) {
				classes[i] = Boolean.TYPE;
				args[i] = new Boolean(true);
			    }
			    else if (helper==Constants.dmlfalse) {
				classes[i] = Boolean.TYPE;
				args[i] = new Boolean(false);
			    }
			    else {
				DMLValue[] err = {
				    new de.uni_sb.ps.dml.runtime.String("illegal Name-argument #"+(i+2)),
				    val};
				return Constants.javaAPIError.apply(new Tuple(err)).raise();
			    }
			}
			else if (helper instanceof JObject) { // Argument ist Instanz einer Java-Klasse
			    java.lang.Object o = ((JObject) helper).getObject();
			    classes[i] = o.getClass();
			    args[i] = o;
			}
			else {
			    DMLValue[] err = {
				new de.uni_sb.ps.dml.runtime.String("illegal argument #"+(i+2)),
				val};
			    return Constants.javaAPIError.apply(new Tuple(err)).raise();
			}
		    } // end of for
		} // end of else Methode
		// classes ist jetzt berechnet, methname bekannt
		// Methode wird laut API-Dokumentation aufgelöst
		// siehe:
		// http://oo/javadocs/jdk1.2.1/docs/api/java/lang/Class.html#getMethod(java.lang.String, java.lang.Class[])
		try {
		    meth = cl.getMethod(methname,classes);
		} catch (NoSuchMethodException e) {
		    try {
			meth = findMethod(methname,cl,classes);
		    } catch (NoSuchMethodException f) {
			DMLValue[] err = {
			    new de.uni_sb.ps.dml.runtime.String(f.getMessage()),
			    new JObject(f)};
			return Constants.javaAPIError.apply(new Tuple(err)).raise();
		    }
		}

		Object oo = null;
		try {
		    oo = meth.invoke(object,args);
		} catch (Exception e) {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String(e.getMessage()),
			new JObject(e)};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
		// ERFOLGREICH !
		// jetzt zurückmappen von java-Objekten nach DML
		if (oo instanceof Boolean)
		    if (((Boolean) oo).booleanValue())
			return Constants.dmltrue;
		    else
			return Constants.dmlfalse;
		else if (oo instanceof java.lang.Integer)
		    return new Int(((java.lang.Integer) oo).intValue());
		else if (oo instanceof Long)
		    return new de.uni_sb.ps.dml.runtime.Word(((Long) oo).longValue());
		else if (oo instanceof Float)
		    return new de.uni_sb.ps.dml.runtime.Real(((Float) oo).floatValue());
		else if (oo instanceof java.lang.String)
		    return new de.uni_sb.ps.dml.runtime.String((java.lang.String) oo);
		else
		    return new JObject(oo);
	    }
	    else {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument for primitve method new_instance"),
		    val};
		return Constants.runtimeError.apply(new Tuple(err)).raise();
	    }
	}


	/** Noch ein Fix für Linux; versucht sich an der Solaris-Version
	 *  zu orientieren.
	 */
	final public static Method findMethod(java.lang.String methname, Class cl, Class[] classes)
	    throws NoSuchMethodException {
	    // System.out.println("Hello find!");
	    Method meth = null;
	    Method[] meths = cl.getMethods();

	    // System.out.println("Arguments for "+methname+"are: ");
	    // for(int i=0; i<classes.length; i++) System.out.println("\t"+classes[i]);
	    // System.out.println("Found Methods for "+cl.getName());
	    // for(int i=0; i<meths.length; i++) System.out.println("\t"+meths[i]);

	    java.util.Vector methfits = new java.util.Vector();
	    int argLength = classes.length;
	    // Die Methods mit Namen und passender Argumentzahl ausssuchen:
	    int length=meths.length;
	    for(int i=0; i<length; i++)
		if (methname.equals(meths[i].getName()) &&
		    meths[i].getParameterTypes().length==argLength)
		    methfits.addElement(meths[i]);

	    // System.out.println("Length-matching Methods for "+cl.getName());
	    // System.out.println("\t"+methfits);

	    java.util.Enumeration enum = methfits.elements();
	    methfits = new java.util.Vector();
	    Method c = null;
	    // alle Methods mit möglicherweise passenden Argumenten
	    while (enum.hasMoreElements()) {
		c = (Method) enum.nextElement();
		// System.out.println("Working on: "+c);
		Class[] args = c.getParameterTypes();
		// Test auf exakten Match redundant, das passiert beim 1. Versuch
		//  	    boolean exact = true; // paßt diese Methode genau?
		//  	    for(int i=0; i<argLength; i++)
		//  		if (!args[i].equals(classes[i])) {
		//  		    System.out.println(args[i]+" doesn't equal "+classes[i]);
		//  		    exact = false;
		//  		    break;
		//  		}
		//  	    if (exact) { // wenn's genau paßt, können wir den zurückgeben
		//  		System.out.println("Exact match: "+c);
		//  		return c;
		//  	    }
		boolean could = true;
		for(int i=0; i<argLength; i++)
		    if (args[i].isAssignableFrom(classes[i])) {
			// classes[i] gleiche oder oberklasse von args
			// System.out.println(args[i]+" assignable from "+classes[i]);
		    }
		    else {
			// System.out.println(args[i]+" not assignable from "+classes[i]);
			could = false;
			break;
		    }
		if (could) {
		    // System.out.println("This one could be used: "+c);
		    methfits.addElement(c);
		}
	    } // hiernach stehen in methfits nur die möglicherweise passenden Methoden
	    // System.out.println("After filtering: "+methfits);
	    int size = methfits.size();
	    if (size==0)
		throw new NoSuchMethodException();
	    else if (size==1)
		return (Method) methfits.elementAt(0);
	    else {
		// suche nach 'most specific method'
		// alle durchgehen, mit allen anderen Vergleichen.
		// falls eine genauere Methode gefunden wird, die ungenauere entfernen

		for(int j=0; j<methfits.size(); j++) {
		    c = (Method) methfits.elementAt(j);
		    Class[] argC = c.getParameterTypes();
		    enum = methfits.elements();
		    while (enum.hasMoreElements()) {
			Method d = (Method) enum.nextElement();

			// System.out.println("Teste "+c+" gegen "+d);
			// System.out.println("\nj="+j+"\tMethstr: "+c);

			if (c==d) {// gleiche Methode
			    // System.out.println("skipping");
			    continue;
			}
			Class[] argD = d.getParameterTypes();
			// kann eine Methode alles besser, was die andere kann, fliegt sie raus
			boolean ungenauer = true;
			for(int i=0; i<argLength; i++) {
			    // argD isAssignableFrom argC <=> argD superklasse von argC
			    // => argD 'ungenauer' als argC
			    // System.out.print("Ist "+argD[i]+" genauer als "+argC[i]+"? ");
			    if (!argD[i].isAssignableFrom(argC[i])) {
				// System.out.println("JA!");
				ungenauer = false;
				break;
			    } // falls argC genauer als argD -> weg mit argD
			    else
				; // System.out.println("NEIN!");
			}
			if (ungenauer) {
			    // System.out.println(c+" ist genauer als "+d);
			    methfits.removeElement(d);
			    j--; // Korrektur im Vector
			    // System.out.println(methfits);
			}
		    }
		}
		// jetzt darf nur eine speziellste übrig bleiben
		if (methfits.size()!=1) {
		    // Fehler !
		    // throw new NoSuchMethodException("ambiguous method call:"+methfits.size());
		    return (Method) methfits.firstElement();
		} else
		    return (Method) methfits.firstElement();
	    }
	}
    }

    final public static InvokeMethod invokeMethod = new InvokeMethod();


/** Builtin zur Erzeugung von java API Objekten.
 *  @author Daniel
 */
final protected static class NewInstance extends Builtin {
    /** Methode entspricht der Konstruktor-Applikation
     *  new_instance: string * arg0 * ... * argN -> JObject
     *  @param v v sollte ein Tupel sein mit
     *  @param #1 de.uni_sb.ps.dml.runtime.String als Klassenname
     *  @param args Argumente für den Konstruktor, kann leer sein
     */
    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	val = val.request();
	if (val instanceof DMLTuple) {
	    DMLTuple v = (DMLTuple) val;
	    DMLValue c = v.getByIndex(0).request(); // erstes Argument
	    if (c instanceof de.uni_sb.ps.dml.runtime.String) {
		java.lang.String classname = ((de.uni_sb.ps.dml.runtime.String) c).getString();
		Class cl = null;
		try {
		    cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String("cannot find class "+classname),
			val};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
		// Klasse gefunden, Class steht in cl
		int length = v.getArity()-1; // Anzahl der Argumente für Konstruktor
		Class[] classes = null;
		Object[] args   = null;
		java.lang.reflect.Constructor con = null;
		if (length==0 || v.getByIndex(1).request()==Constants.dmlunit) { // Nullstelliger Konstruktor
		    try {
			con = cl.getConstructor(null);
		    } catch (NoSuchMethodException e) {
			DMLValue[] err = {
			    new de.uni_sb.ps.dml.runtime.String("no nullary constructor available for "+classname),
			    val};
			return Constants.javaAPIError.apply(new Tuple(err)).raise();
		    }
		}
		else { // Konstruktor hat >= 1 Argumente
		    classes = new Class[length];
		    args = new java.lang.Object[length];
		    for(int i=0; i<length; i++) {
			DMLValue helper = v.getByIndex(i+1).request();
			if (helper instanceof SCon) {
			    // Argument ist de.uni_sb.ps.dml.runtime.String, Int, Real oder de.uni_sb.ps.dml.runtime.Word
			    if (helper instanceof Int) {
				classes[i] = java.lang.Integer.TYPE;
				args[i] = new java.lang.Integer(((Int) helper).getInt());
			    }
			    else if (helper instanceof de.uni_sb.ps.dml.runtime.Real) {
				classes[i] = Float.TYPE;
				args[i] = new Float(((de.uni_sb.ps.dml.runtime.Real) helper).getFloat());
			    }
			    else if (helper instanceof de.uni_sb.ps.dml.runtime.String) {
				try{
				    classes[i] = Class.forName("java.lang.String");  // java.lang.String.TYPE;
				} catch (ClassNotFoundException e) {
				    // This should NEVER happen
				    System.err.println("internal runtime error: new_instance");
				    System.err.println(e.getMessage());
				    e.printStackTrace();
				}
				args[i] = ((de.uni_sb.ps.dml.runtime.String) helper).getString();
			    }
			    else if (helper instanceof de.uni_sb.ps.dml.runtime.Word) {
				classes[i] = Long.TYPE;
				args[i] = new Long(((de.uni_sb.ps.dml.runtime.Word) helper).getLong());
			    }
			}
			else if (helper instanceof Name) {
			    if (helper==Constants.dmltrue) {
				classes[i] = Boolean.TYPE;
				args[i] = new Boolean(true);
			    }
			    else if (helper==Constants.dmlfalse) {
				classes[i] = Boolean.TYPE;
				args[i] = new Boolean(false);
			    }
			    else {
				DMLValue[] err = {
				    new de.uni_sb.ps.dml.runtime.String("illegal Name-argument #"+i),
				    v};
				return Constants.javaAPIError.apply(new Tuple(err)).raise();
			    }
			}
			else if (helper instanceof JObject) {
			    // Argument ist Instanz einer Java-Klasse
			    java.lang.Object o = ((JObject) helper).getObject();
			    classes[i] = o.getClass();
			    args[i] = o;
			}
			else {
			    DMLValue[] err = {
				new de.uni_sb.ps.dml.runtime.String("illegal argument #"+i),
				v};
			    return Constants.javaAPIError.apply(new Tuple(err)).raise();
			}
			} // end of for
		    }
		try {
		    con = cl.getConstructor(classes); // wenn genau paßt: Redundanz vermeiden mit findConstructor
		    // System.out.println(con);
		} catch (NoSuchMethodException e) {
		    try {
			con = findConstructor(cl,classes);
		    } catch (NoSuchMethodException f) {
			DMLValue[] err = {
			    new de.uni_sb.ps.dml.runtime.String(f.getMessage()),
			    new JObject(f)};
			return Constants.javaAPIError.apply(new Tuple(err)).raise();
		    }
		}
		
		Object oo = null;
		try {
		    oo = con.newInstance(args);
		} catch (Exception e) {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String(e.getMessage()),
			new JObject(e)};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
		// ERFOLGREICH !
		// zurückmappen
		if (oo instanceof Boolean)
		    if (((Boolean) oo).booleanValue())
			return Constants.dmltrue;
		    else
			return Constants.dmlfalse;
		else if (oo instanceof java.lang.Integer)
		    return new Int(((java.lang.Integer) oo).intValue());
		else if (oo instanceof Long)
		    return new de.uni_sb.ps.dml.runtime.Word(((Long) oo).longValue());
		else if (oo instanceof Float)
		    return new de.uni_sb.ps.dml.runtime.Real(((Float) oo).floatValue());
		else if (oo instanceof java.lang.String)
		    return new de.uni_sb.ps.dml.runtime.String((java.lang.String) oo);
		else
		    return new JObject(oo);
	    } // end of: if (c instanceof de.uni_sb.ps.dml.runtime.String)
	    else {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument for classtype"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }
	}
	else {
	    DMLValue[] err = {
		new de.uni_sb.ps.dml.runtime.String("illegal argument for primitve method new_instance"),
		val};
	    return Constants.runtimeError.apply(new Tuple(err)).raise();
	}
    }

    /** Dynamische Auswahl eines Konstruktors via Reflect.
     *  Patch für Linux; auf Solaris laufen wir da nie rein ...
     *  Entspricht hoffentlich der Vorgehensweise wie in  <a href="http://oo.ps.uni-sb.de/javadocs/langspec-1.0/15.doc.html#21693">Java Sprachdefinition</a>
     */
    final static private Constructor findConstructor(Class cl, Class[] classes) throws NoSuchMethodException {
	// System.out.println("Hello find!");
	Constructor con = null;
	Constructor[] cons = cl.getConstructors();

	// System.out.println("Arguments are: ");
	// for(int i=0; i<classes.length; i++) System.out.println("\t"+classes[i]);
	// System.out.println("Found constructors for "+cl.getName());
	// for(int i=0; i<cons.length; i++) System.out.println("\t"+cons[i]);

	java.util.Vector confits = new java.util.Vector();
	int argLength = classes.length;
	// Die Constructors mit passender Argumentzahl ausssuchen:
	int length=cons.length;
	for(int i=0; i<length; i++)
	    if (cons[i].getParameterTypes().length==argLength)
		confits.addElement(cons[i]);

	// System.out.println("Length-matching constructors for "+cl.getName());
	// System.out.println("\t"+confits);

	java.util.Enumeration enum = confits.elements();
	confits = new java.util.Vector();
	Constructor c = null;
	// alle Constructors mit möglicherweise passenden Argumenten
	while (enum.hasMoreElements()) {
	    c = (Constructor) enum.nextElement();
	    // System.out.println("Working on: "+c);
	    Class[] args = c.getParameterTypes();
	    // Test auf exakten Match redundant, das passiert beim 1. Versuch
	    //  	    boolean exact = true; // paßt diese Methode genau?
	    //  	    for(int i=0; i<argLength; i++)
	    //  		if (!args[i].equals(classes[i])) {
	    //  		    System.out.println(args[i]+" doesn't equal "+classes[i]);
	    //  		    exact = false;
	    //  		    break;
	    //  		}
	    //  	    if (exact) { // wenn's genau paßt, können wir den zurückgeben
	    //  		System.out.println("Exact match: "+c);
	    //  		return c;
	    //  	    }
	    boolean could = true;
	    for(int i=0; i<argLength; i++)
		if (args[i].isAssignableFrom(classes[i])) {
		    // classes[i] gleiche oder oberklasse von args
		    // System.out.println(args[i]+" assignable from "+classes[i]);
		}
		else {
		    // System.out.println(args[i]+" not assignable from "+classes[i]);
		    could = false;
		    break;
		}
	    if (could) {
		// System.out.println("This one could be used: "+c);
		confits.addElement(c);
	    }
	} // hiernach stehen in confits nur die möglicherweise passenden Constructoren
	// System.out.println("After filtering: "+confits);
	int size = confits.size();
	if (size==0)
	    throw new NoSuchMethodException();
	else if (size==1)
	    return (Constructor) confits.elementAt(0);
	else { // suche nach 'most specific method'
	    // alle durchgehen, mit allen anderen Vergleichen.
	    // falls eine genauere Methode gefunden wird, die ungenauere entfernen

	    for(int j=0; j<confits.size(); j++) {
		c = (Constructor) confits.elementAt(j);
		Class[] argC = c.getParameterTypes();
		enum = confits.elements();
		while (enum.hasMoreElements()) {
		    Constructor d = (Constructor) enum.nextElement();

		    // System.out.println("Teste "+c+" gegen "+d);
		    // System.out.println("\nj="+j+"\tConstr: "+c);

		    if (c==d) {// gleicher Constructor
			// System.out.println("skipping");
			continue;
		    }
		    Class[] argD = d.getParameterTypes();
		    // kann eine Methode alles besser, was die andere kann, fliegt sie raus
		    boolean ungenauer = true;
		    for(int i=0; i<argLength; i++) {
			// argD isAssignableFrom argC <=> argD superklasse von argC
			// => argD 'ungenauer' als argC
			// System.out.print("Ist "+argD[i]+" genauer als "+argC[i]+"? ");
			if (!argD[i].isAssignableFrom(argC[i])) {
			    // System.out.println("JA!");
			    ungenauer = false;
			    break;
			} // falls argC genauer als argD -> weg mit argD
			else
			    ; // System.out.println("NEIN!");
		    }
		    if (ungenauer) {
			// System.out.println(c+" ist genauer als "+d);
			confits.removeElement(d);
			j--; // Korrektur im Vector
			// System.out.println(confits);
		    }
		}
	    }
	    // jetzt darf nur eine speziellste übrig bleiben
	    if (confits.size()!=1) {
		// Fehler ! (eigentlich nach Sprachspezfikation
		// throw new NoSuchMethodException("ambiguous method call:"+confits.size());
		return (Constructor) confits.firstElement();
	    } else
		return (Constructor) confits.firstElement();
	}
    }
}
final public static NewInstance newInstance = new NewInstance();


/** Builtin zum Setzen von java API Objekt/Klassen-Feldern
 *  @author Daniel
 */
final protected static class Putfield extends Builtin {
    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	val = val.request();
	if (val instanceof DMLTuple) {
	    DMLTuple v = (DMLTuple) val;
	    if (v.getArity()!=3) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("wrong number of arguments for putfield"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }
	    // else:   
	    DMLValue c = v.getByIndex(0).request(); // java.lang.Object oder Klasse
	    
	    Class cl = null;
	    java.lang.Object object = null;
	    java.lang.String classname = null;
	    if (c instanceof de.uni_sb.ps.dml.runtime.String) { // nur noch Klassenfelder
		classname = ((de.uni_sb.ps.dml.runtime.String) c).getString();
		try {
		    cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String("cannot find class "+classname),
			val};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
	    } else if (c instanceof JObject) {
		object = ((JObject) c).getObject();
		cl = object.getClass();
		classname=cl.getName();
	    } else { // Fehler
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument for classtype"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }
	    // Klasse gefunden, Class steht in cl
	    // Objekt steht in object

	    c = v.getByIndex(1).request(); // hier: Feldname
	    java.lang.String fieldname = null;
	    if (c instanceof de.uni_sb.ps.dml.runtime.String) 
		fieldname = ((de.uni_sb.ps.dml.runtime.String) c).getString();
	    else {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument for fieldname"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }

	    c = v.getByIndex(2).request(); // hier: Wert
	    java.lang.Object arg = null;
	    if (c instanceof SCon) {
		// Argument ist de.uni_sb.ps.dml.runtime.String, Int, Real oder de.uni_sb.ps.dml.runtime.Word
		if (c instanceof Int) {
		    arg = new java.lang.Integer(((Int) c).getInt());
		}
		else if (c instanceof de.uni_sb.ps.dml.runtime.Real) {
		    arg = new Float(((de.uni_sb.ps.dml.runtime.Real) c).getFloat());
		}
		else if (c instanceof de.uni_sb.ps.dml.runtime.String) {
		    arg = ((de.uni_sb.ps.dml.runtime.String) c).getString();
		}
		else if (c instanceof de.uni_sb.ps.dml.runtime.Word) {
		    arg = new Long(((de.uni_sb.ps.dml.runtime.Word) c).getLong());
		}
	    }
	    else if (c instanceof Name) {
		if (c==Constants.dmltrue) {
		    arg = new Boolean(true);
		}
		else if (c==Constants.dmlfalse) {
		    arg = new Boolean(false);
		}
		else {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String("illegal Name-argument for putfield"),
			val};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
	    }
	    else if (c instanceof JObject) {
		// Argument ist Instanz einer Java-Klasse
		arg = ((JObject) c).getObject();
	    }
	    else {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument #3 for putfield"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }

	    // jetzt koennen wir
	    Field field = null;
	    try {
		field = cl.getField(fieldname);
	    } catch (NoSuchFieldException e) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String(e.getMessage()),
		    new JObject(e)};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }

	    try {
		field.set(object,arg);
	    } catch (IllegalAccessException e) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String(e.getMessage()),
		    new JObject(e)};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    } catch (Exception e) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String(e.getMessage()),
		    new JObject(e)};
		return Constants.javaAPIException.apply(new Tuple(err)).raise();
	    }
	    return Constants.dmlunit; // wie bei ':='
	}
	else {
	    DMLValue[] err = {
		new de.uni_sb.ps.dml.runtime.String("illegal argument for fieldname"),
		val};
	    return Constants.javaAPIError.apply(new Tuple(err)).raise();
	}
    }
}
    final public static Putfield putfield = new Putfield();


/** Builtin zum Setzen von java API Objekt/Klassen-Feldern
 *  @author Daniel
 */
final protected static class Getfield extends Builtin {
    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	val = val.request();
	if (val instanceof DMLTuple) {
	    DMLTuple v = (DMLTuple) val;
	    if (v.getArity()!=2) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("wrong number of arguments for getfield"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }
	    // else:   
	    DMLValue c = v.getByIndex(0).request(); // java.lang.Object oder Klasse

	    Class cl = null;
	    java.lang.Object object = null;
	    java.lang.String classname = null;
	    if (c instanceof de.uni_sb.ps.dml.runtime.String) { // nur noch Klassenfelder
		classname = ((de.uni_sb.ps.dml.runtime.String) c).getString();
		try {
		    cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String("cannot find class "+classname),
			val};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
	    } else if (c instanceof JObject) {
		object = ((JObject) c).getObject();
		cl = object.getClass();
		classname=cl.getName();
	    } else { // Fehler
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument for classtype"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }
	    // Klasse gefunden, Class steht in cl
	    // Objekt steht in object (evtl. null)

	    c = v.getByIndex(1).request(); // hier: Feldname
	    java.lang.String fieldname = null;
	    if (c instanceof de.uni_sb.ps.dml.runtime.String)
		fieldname = ((de.uni_sb.ps.dml.runtime.String) c).getString();
	    else {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument for fieldname"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }

	    // jetzt koennen wir
	    Field field = null;
	    try {
		field = cl.getField(fieldname);
	    } catch (NoSuchFieldException e) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String(e.getMessage()),
		    new JObject(e)};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }

	    java.lang.Object oo = null;
	    try {
		oo = field.get(object);
	    } catch (IllegalAccessException e) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String(e.getMessage()),
		    new JObject(e)};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    } catch (Exception e) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String(e.getMessage()),
		    new JObject(e)};
		return Constants.javaAPIException.apply(new Tuple(err)).raise();
	    }
	    // Erfolgreich durchgeführt ...
	    // zurückmappen
	    if (oo instanceof Boolean)
		if (((Boolean) oo).booleanValue())
		    return Constants.dmltrue;
		else
		    return Constants.dmlfalse;
	    else if (oo instanceof java.lang.Integer)
		return new Int(((java.lang.Integer) oo).intValue());
	    else if (oo instanceof Long)
		return new de.uni_sb.ps.dml.runtime.Word(((Long) oo).longValue());
	    else if (oo instanceof Float)
		return new de.uni_sb.ps.dml.runtime.Real(((Float) oo).floatValue());
	    else if (oo instanceof java.lang.String)
		return new de.uni_sb.ps.dml.runtime.String((java.lang.String) oo);
	    else
		return new JObject(oo);
	}
	else {
	    DMLValue[] err = {
		new de.uni_sb.ps.dml.runtime.String("illegal argument for fieldname"),
		val};
	    return Constants.javaAPIError.apply(new Tuple(err)).raise();
	}
    }
}
    final public static Getfield getfield = new Getfield();

/** Builtin zum instanceof testen
 *  @author Daniel
 */
final protected static class InstanceOf extends Builtin {
    /** Methode entspricht der Applikation einer Methode
     *  invoke: string/JObject * string * arg0 * ... * argN -> JObject
     *  @param v v sollte ein Tupel sein mit
     *  @param #1 de.uni_sb.ps.dml.runtime.String als Klassenname oder
     *  @param #1 JObject mit einem java.lang.Object der Klasse
     *  @param #2 wie #1
     */
    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	val = val.request();
	if (val instanceof DMLTuple) {
	    DMLTuple v = (DMLTuple) val;
	    if (v.getArity()!=2) {
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("invalid number of arguments for instanceOf"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }
	    DMLValue c = v.getByIndex(0).request(); // erstes Argument ist java.lang.Object oder Klasse
	    
	    // 1. Argument
	    Class cl = null;
	    if (c instanceof de.uni_sb.ps.dml.runtime.String) {
		java.lang.String classname = ((de.uni_sb.ps.dml.runtime.String) c).getString();
		try {
		    cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String("cannot find class "+classname),
			val};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
	    } else if (c instanceof JObject) {
		cl = ((JObject) c).getObject().getClass();
	    } else { // Fehler
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument #1 for instanceOf"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }

	    // 2. Argument
	    c = v.getByIndex(1).request(); // zweites Argument
	    Class cl2 = null;
	    if (c instanceof de.uni_sb.ps.dml.runtime.String) {
		java.lang.String classname = ((de.uni_sb.ps.dml.runtime.String) c).getString();
		try {
		    cl2 = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    DMLValue[] err = {
			new de.uni_sb.ps.dml.runtime.String("cannot find class "+classname),
			val};
		    return Constants.javaAPIError.apply(new Tuple(err)).raise();
		}
	    } else if (c instanceof JObject) {
		cl2 = ((JObject) c).getObject().getClass();
	    } else { // Fehler
		DMLValue[] err = {
		    new de.uni_sb.ps.dml.runtime.String("illegal argument #2 for instanceOf"),
		    val};
		return Constants.javaAPIError.apply(new Tuple(err)).raise();
	    }
	    
	    if (cl2.isAssignableFrom(cl))
		return Constants.dmltrue;
	    else
		return Constants.dmlfalse;
	}
	else {
	    DMLValue[] err = {
		new de.uni_sb.ps.dml.runtime.String("illegal argument #2 for instanceOf"),
		val};
	    return Constants.javaAPIError.apply(new Tuple(err)).raise();
	}
    }
}
final public static InstanceOf instanceOf = new InstanceOf();
}
