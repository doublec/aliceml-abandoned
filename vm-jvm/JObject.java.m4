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
    _BUILTIN(InvokeMethod) {
	/** Methode entspricht der Applikation einer Methode
	 *  invoke: string/JObject * string * arg0 * ... * argN -> JObject
	 *  @param v v sollte ein Tupel sein mit
	 *  @param 1 STRING  als Klassenname oder
	 *  @param 1 JObject mit einem java.lang.Object der Klasse
	 *  @param args Argumente für die Methode, kann leer sein
	 */
	_APPLY(val) {
	    if (val instanceof DMLTuple) {
		DMLTuple v = (DMLTuple) val;
		_REQUESTDEC(DMLValue c,v.getByIndex(0)); // erstes Argument ist java.lang.Object oder Klasse

		Class cl = null;
		Object object = null;
		java.lang.String classname = null;
		if (c instanceof STRING) { // nur noch Klasse
		    classname = ((STRING) c).getString();
		    try {
			cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		    } catch (ClassNotFoundException e) {
			_RAISE(javaAPIError,new Tuple2(new STRING ("cannot find class "+classname),val));
		    }
		} else if (c instanceof JObject) {
		    object = ((JObject) c).getObject();
		    cl = object.getClass();
		    classname=cl.getName();
		} else { // Fehler
		    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for classtype"),val));
		}
		// Klasse gefunden, Class steht in cl

		_REQUESTDEC(DMLValue mn,v.getByIndex(1)); // Methodenname
		java.lang.String methname = null;
		if (!(mn instanceof STRING)) {
		    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for methodname"),						   val));
		}
		else {
		    methname = ((STRING) mn).getString();
		}
		// System.out.println("Object: "+classname+" Method: "+methname);
		int length = v.getArity()-2; // Anzahl der Argumente für Methode
		Class[] classes = null;
		Object[] args   = null;
		java.lang.reflect.Method meth = null;
		_REQUESTDEC(DMLValue unit,v.getByIndex(2));
		if (length==0 || unit==Constants.dmlunit) { // nullstellige Methode
		    try {
			meth = cl.getMethod(methname,null);
		    } catch (NoSuchMethodException e) {
			_RAISE(javaAPIError,new Tuple2(new STRING ("no nullary method available for "+classname),						       val));
		    }
		}
		else { // Methode hat >= 1 Argumente
		    classes = new Class[length];
		    args = new java.lang.Object[length];
		    for(int i=0; i<length; i++) {
			_REQUESTDEC(DMLValue helper,v.getByIndex(i+2));
			if (helper instanceof Int) {
			    classes[i] = java.lang.Integer.TYPE;
			    args[i] = new java.lang.Integer(((Int) helper).getInt());
			} else if (helper instanceof Real) {
			    classes[i] = Float.TYPE;
			    args[i] = new Float(((Real) helper).getFloat());
			} else if (helper instanceof STRING) {
			    try{
				classes[i] = Class.forName("java.lang.String");  // java.lang.String.TYPE;
			    } catch (ClassNotFoundException e) {
				// This should NEVER happen
				System.err.println("internal runtime error: new_instance");
				System.err.println(e.getMessage());
				e.printStackTrace();
			    }
			    args[i] = ((STRING) helper).getString();
			} else if (helper instanceof Word) {
			    classes[i] = Long.TYPE;
			    args[i] = new Long(((Word) helper).getLong());
			} else if (helper instanceof Name) {
			    if (helper==Constants.dmltrue) {
				classes[i] = Boolean.TYPE;
				args[i] = new Boolean(true);
			    } else if (helper==Constants.dmlfalse) {
				classes[i] = Boolean.TYPE;
				args[i] = new Boolean(false);
			    } else {
				_RAISE(javaAPIError,new Tuple2(new STRING ("illegal Name-argument "+(i+2)),							       val));
			    }
			} else if (helper instanceof JObject) { // Argument ist Instanz einer Java-Klasse
			    java.lang.Object o = ((JObject) helper).getObject();
			    classes[i] = o.getClass();
			    args[i] = o;
			} else {
			    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument "+(i+2)),							   val));
			}
		    } // end of for
		} // end of else Methode
		// classes ist jetzt berechnet, methname bekannt
		// Methode wird laut API-Dokumentation aufgelöst
		// siehe:
		// http://oo/javadocs/jdk1.2.1/docs/api/java/lang/Class.htmlgetMethod(java.lang.String, java.lang.Class[])
		try {
		    meth = cl.getMethod(methname,classes);
		} catch (NoSuchMethodException e) {
		    try {
			meth = findMethod(methname,cl,classes);
		    } catch (NoSuchMethodException f) {
			_RAISE(javaAPIError,new Tuple2(new STRING (f.getMessage()),						       new JObject(f)));
		    }
		}

		Object oo = null;
		try {
		    oo = meth.invoke(object,args);
		} catch (Exception e) {
		    _RAISE(javaAPIError,new Tuple2(new STRING (e.getMessage()),						   new JObject(e)));
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
		    return new Word(((Long) oo).longValue());
		else if (oo instanceof Float)
		    return new Real(((Float) oo).floatValue());
		else if (oo instanceof java.lang.String)
		    return new STRING ((java.lang.String) oo);
		else
		    return new JObject(oo);
	    }
	    else {
		_RAISE(runtimeError,new Tuple2(new STRING ("illegal argument for primitve method new_instance"),
					       val));
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

    _FIELD(JObject,invokeMethod);

    /** Builtin zur Erzeugung von java API Objekten.
 *  @author Daniel
 */
    _BUILTIN(NewInstance) {
	/** Methode entspricht der Konstruktor-Applikation
	 *  new_instance: string * arg0 * ... * argN -> JObject
	 *  @param v v sollte ein Tupel sein mit
	 *  @param 1 STRING  als Klassenname
	 *  @param args Argumente für den Konstruktor, kann leer sein
	 */
	_APPLY(val) {
	    _REQUEST(val,val);
	    if (val instanceof DMLTuple) {
		DMLTuple v = (DMLTuple) val;
		_REQUESTDEC(DMLValue c,v.getByIndex(0)); // erstes Argument
		if (c instanceof STRING) {
		    java.lang.String classname = ((STRING) c).getString();
		    Class cl = null;
		    try {
			cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		    } catch (ClassNotFoundException e) {
			_RAISE(javaAPIError,new Tuple2(new STRING ("cannot find class "+classname),
						       val));
		    }
		    // Klasse gefunden, Class steht in cl
		    int length = v.getArity()-1; // Anzahl der Argumente für Konstruktor
		    Class[] classes = null;
		    Object[] args   = null;
		    java.lang.reflect.Constructor con = null;
		    _REQUESTDEC(DMLValue unit,v.getByIndex(1));
		    if (length==0 || unit==Constants.dmlunit) { // Nullstelliger Konstruktor
			try {
			    con = cl.getConstructor(null);
			} catch (NoSuchMethodException e) {
			    _RAISE(javaAPIError,new Tuple2(new STRING ("no nullary constructor available for "+classname), val));
			}
		    }
		    else { // Konstruktor hat >= 1 Argumente
			classes = new Class[length];
			args = new java.lang.Object[length];
			for(int i=0; i<length; i++) {
			    _REQUESTDEC(DMLValue helper,v.getByIndex(i+1));
			    if (helper instanceof Int) {
				classes[i] = java.lang.Integer.TYPE;
				args[i] = new java.lang.Integer(((Int) helper).getInt());
			    } else if (helper instanceof Real) {
				classes[i] = Float.TYPE;
				args[i] = new Float(((Real) helper).getFloat());
			    } else if (helper instanceof STRING) {
				try{
				    classes[i] = Class.forName("java.lang.String");  // java.lang.String.TYPE;
				} catch (ClassNotFoundException e) {
				    // This should NEVER happen
				    System.err.println("internal runtime error: new_instance");
				    System.err.println(e.getMessage());
				    e.printStackTrace();
				}
				args[i] = ((STRING) helper).getString();
			    } else if (helper instanceof Word) {
				classes[i] = Long.TYPE;
				args[i] = new Long(((Word) helper).getLong());
			    } else if (helper instanceof Name) {
				if (helper==Constants.dmltrue) {
				    classes[i] = Boolean.TYPE;
				    args[i] = new Boolean(true);
				} else if (helper==Constants.dmlfalse) {
				    classes[i] = Boolean.TYPE;
				    args[i] = new Boolean(false);
				} else {
				    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal Name-argument "+i),
								   v));
				}
			    } else if (helper instanceof JObject) {
				// Argument ist Instanz einer Java-Klasse
				java.lang.Object o = ((JObject) helper).getObject();
				classes[i] = o.getClass();
				args[i] = o;
			    } else {
				_RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument "+i),
							       v));
			    }
			} // end of for
		    }
		    try {
			con = cl.getConstructor(classes); // wenn genau passt: Redundanz vermeiden mit findConstructor
			// System.out.println(con);
		    } catch (NoSuchMethodException e) {
			try {
			    con = findConstructor(cl,classes);
			} catch (NoSuchMethodException f) {
			    _RAISE(javaAPIError,new Tuple2(new STRING (f.getMessage()),
							   new JObject(f)));
			}
		    }

		    Object oo = null;
		    try {
			oo = con.newInstance(args);
		    } catch (Exception e) {
			_RAISE(javaAPIError,new Tuple2(new STRING (e.getMessage()),
						       new JObject(e)));
		    }
		    // ERFOLGREICH !
		    // zurückmappen
		    if (oo instanceof Boolean)
			if (((Boolean) oo).booleanValue())
			    return Constants.dmltrue;
			else
			    return Constants.dmlfalse;
		    else if (oo instanceof Integer)
			return new Int(((Integer) oo).intValue());
		    else if (oo instanceof Long)
			return new Word(((Long) oo).longValue());
		    else if (oo instanceof Float)
			return new Real(((Float) oo).floatValue());
		    else if (oo instanceof java.lang.String)
			return new STRING ((java.lang.String) oo);
		    else
			return new JObject(oo);
		} // end of: if (c instanceof STRING)
		else {
		    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for classtype"),
						   val));
		}
	    }
	    else {
		_RAISE(runtimeError,new Tuple2(new STRING ("illegal argument for primitve method new_instance"),
					       val));
	    }
	}

	/** Dynamische Auswahl eines Konstruktors via Reflect.
	 *  Patch für Linux; auf Solaris laufen wir da nie rein ...
	 *  Entspricht hoffentlich der Vorgehensweise wie in  <a href="http://oo.ps.uni-sb.de/javadocs/langspec-1.0/15.doc.html21693">Java Sprachdefinition</a>
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
    _FIELD(JObject,newInstance);


/** Builtin zum Setzen von java API Objekt/Klassen-Feldern
 *  @author Daniel
 */
_BUILTIN(Putfield) {
    _APPLY(val) {
	_REQUEST(val,val);
	if (val instanceof DMLTuple) {
	    DMLTuple v = (DMLTuple) val;
	    if (v.getArity()!=3) {
		_RAISE(javaAPIError,new Tuple2(new STRING ("wrong number of arguments for putfield"),
					       val));
	    }
	    // else:   
	    _REQUESTDEC(DMLValue c,v.getByIndex(0)); // java.lang.Object oder Klasse

	    Class cl = null;
	    java.lang.Object object = null;
	    java.lang.String classname = null;
	    if (c instanceof STRING) { // nur noch Klassenfelder
		classname = ((STRING) c).getString();
		try {
		    cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    _RAISE(javaAPIError,new Tuple2(new STRING ("cannot find class "+classname),
						   val));
		}
	    } else if (c instanceof JObject) {
		object = ((JObject) c).getObject();
		cl = object.getClass();
		classname=cl.getName();
	    } else { // Fehler
		DMLValue[] err = {
		    };
		_RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for classtype"),
					       val));
	    }
	    // Klasse gefunden, Class steht in cl
	    // Objekt steht in object

	    _REQUEST(c,v.getByIndex(1)); // hier: Feldname
	    java.lang.String fieldname = null;
	    if (c instanceof STRING) 
		fieldname = ((STRING) c).getString();
	    else {
		_RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for fieldname"),
					       val));
	    }

	    _REQUEST(c,v.getByIndex(2)); // hier: Wert
	    java.lang.Object arg = null;
	    if (c instanceof Int) {
		arg = new Integer(((Int) c).getInt());
	    } else if (c instanceof Real) {
		arg = new Float(((Real) c).getFloat());
	    } else if (c instanceof STRING) {
		arg = ((STRING) c).getString();
	    } else if (c instanceof Word) {
		arg = new Long(((Word) c).getLong());
	    } else if (c instanceof Name) {
		if (c==Constants.dmltrue) {
		    arg = new Boolean(true);
		} else if (c==Constants.dmlfalse) {
		    arg = new Boolean(false);
		} else {
		    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal Name-argument for putfield"),
						   val));
		}
	    } else if (c instanceof JObject) {
		// Argument ist Instanz einer Java-Klasse
		arg = ((JObject) c).getObject();
	    } else {
		_RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument 3 for putfield"),
					       val));
	    }

	    // jetzt koennen wir
	    Field field = null;
	    try {
		field = cl.getField(fieldname);
	    } catch (NoSuchFieldException e) {
		_RAISE(javaAPIError,new Tuple2(new STRING (e.getMessage()),
					       new JObject(e)));
	    }

	    try {
		field.set(object,arg);
	    } catch (IllegalAccessException e) {
		DMLValue[] err = {
		    };
		_RAISE(javaAPIError,new Tuple2(new STRING (e.getMessage()),
					       new JObject(e)));
	    } catch (Exception e) {
		_RAISE(javaAPIException,new Tuple2(new STRING (e.getMessage()),
						   new JObject(e)));
	    }
	    return Constants.dmlunit; // wie bei ':='
	}
	else {
	    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for fieldname"),
					   val));
	}
    }
}

 _FIELD(JObject,putfield);


/** Builtin zum Setzen von java API Objekt/Klassen-Feldern
 *  @author Daniel
 */
_BUILTIN(Getfield) {
    _APPLY(val) {
	_REQUEST(val,val);
	if (val instanceof DMLTuple) {
	    DMLTuple v = (DMLTuple) val;
	    if (v.getArity()!=2) {
		_RAISE(javaAPIError,new Tuple2(new STRING ("wrong number of arguments for getfield"),
					       val));
	    }
	    // else:   
	    _REQUESTDEC(DMLValue c,v.getByIndex(0)); // java.lang.Object oder Klasse

	    Class cl = null;
	    java.lang.Object object = null;
	    java.lang.String classname = null;
	    if (c instanceof STRING) { // nur noch Klassenfelder
		classname = ((STRING) c).getString();
		try {
		    cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    _RAISE(javaAPIError,new Tuple2(new STRING ("cannot find class "+classname),
						   val));
		}
	    } else if (c instanceof JObject) {
		object = ((JObject) c).getObject();
		cl = object.getClass();
		classname=cl.getName();
	    } else { // Fehler
		DMLValue[] err = {
		    };
		_RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for classtype"),
					       val));
	    }
	    // Klasse gefunden, Class steht in cl
	    // Objekt steht in object (evtl. null)

	    _REQUEST(c,v.getByIndex(1)); // hier: Feldname
	    java.lang.String fieldname = null;
	    if (c instanceof STRING)
		fieldname = ((STRING) c).getString();
	    else {
		_RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for fieldname"),
					       val));
	    }

	    // jetzt koennen wir
	    Field field = null;
	    try {
		field = cl.getField(fieldname);
	    } catch (NoSuchFieldException e) {
		_RAISE(javaAPIError,new Tuple2(new STRING (e.getMessage()),
					       new JObject(e)));
	    }

	    java.lang.Object oo = null;
	    try {
		oo = field.get(object);
	    } catch (Exception e) {
		_RAISE(javaAPIException,new Tuple2(new STRING (e.getMessage()),
						   new JObject(e)));
	    }
	    // Erfolgreich durchgeführt ...
	    // zurückmappen
	    if (oo instanceof Boolean)
		if (((Boolean) oo).booleanValue())
		    return Constants.dmltrue;
		else
		    return Constants.dmlfalse;
	    else if (oo instanceof java.lang.Integer)
		return new Int(((Integer) oo).intValue());
	    else if (oo instanceof Long)
		return new Word(((Long) oo).longValue());
	    else if (oo instanceof Float)
		return new Real(((Float) oo).floatValue());
	    else if (oo instanceof java.lang.String)
		return new STRING ((java.lang.String) oo);
	    else
		return new JObject(oo);
	}
	else {
	    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument for fieldname"),
					   val));
	}
    }
}
 _FIELD(JObject,getfield);

/** Builtin zum instanceof testen
 *  @author Daniel
 */
_BUILTIN(InstanceOf) {
    /** Methode entspricht der Applikation einer Methode
     *  invoke: string/JObject * string * arg0 * ... * argN -> JObject
     *  @param v v sollte ein Tupel sein mit
     *  @param 1 STRING  als Klassenname oder
     *  @param 1 JObject mit einem java.lang.Object der Klasse
     *  @param 2 wie 1
     */
    _APPLY(val) {
	_REQUEST(val,val);
	if (val instanceof DMLTuple) {
	    DMLTuple v = (DMLTuple) val;
	    if (v.getArity()!=2) {
		_RAISE(javaAPIError,new Tuple2(new STRING ("invalid number of arguments for instanceOf"),
					       val));
	    }
	    _REQUESTDEC(DMLValue c,v.getByIndex(0)); // erstes Argument ist java.lang.Object oder Klasse

	    // 1. Argument
	    Class cl = null;
	    if (c instanceof STRING) {
		java.lang.String classname = ((STRING) c).getString();
		try {
		    cl = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    _RAISE(javaAPIError,new Tuple2(new STRING ("cannot find class "+classname),
						   val));
		}
	    } else if (c instanceof JObject) {
		cl = ((JObject) c).getObject().getClass();
	    } else { // Fehler
		_RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument 1 for instanceOf"),
		    val));
	    }

	    // 2. Argument
	    _REQUEST(c,v.getByIndex(1)); // zweites Argument
	    Class cl2 = null;
	    if (c instanceof STRING) {
		java.lang.String classname = ((STRING) c).getString();
		try {
		    cl2 = ClassLoader.getSystemClassLoader().loadClass(classname);
		} catch (ClassNotFoundException e) {
		    _RAISE(javaAPIError,new Tuple2(new STRING ("cannot find class "+classname),
						   val));
		}
	    } else if (c instanceof JObject) {
		cl2 = ((JObject) c).getObject().getClass();
	    } else { // Fehler
		_RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument 2 for instanceOf"),
					       val));
	    }

	    if (cl2.isAssignableFrom(cl))
		return Constants.dmltrue;
	    else
		return Constants.dmlfalse;
	}
	else {
	    _RAISE(javaAPIError,new Tuple2(new STRING ("illegal argument 2 for instanceOf"),
					   val));
	}
    }
}
 _FIELD(JObject,instanceOf);
}
