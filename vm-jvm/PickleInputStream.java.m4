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

import java.io.*;
import java.lang.reflect.*;

/** This is the InputStream used to read pickles.
 */
final public class PickleInputStream extends ObjectInputStream {

    public PickleInputStream() throws IOException {
	super();
    }

    public PickleInputStream(InputStream in)  throws IOException {
	super(in);
    }

    final protected Class resolveClass(ObjectStreamClass osc) throws IOException, ClassNotFoundException {
	//System.out.println("resolving Class " + osc);
	if (readBoolean()) {
//          System.out.println("reading class from pickle");
	    int length = readInt();
	    byte[] bytes = new byte[length];
	    readFully(bytes,0,length); // NICHT: read
//          try {
//              OutputStream out = new FileOutputStream("in"+osc.getName());
//              DataOutputStream rout = new DataOutputStream(out);
//              out.write(bytes,0,length);
//          } catch (Exception e) {
//              System.err.println("HUCH: "+e);
//          }
//          System.out.println("read "+length+" bytes of "+osc.getName());
	    PickleClassLoader.loader.enter(osc.getName(),bytes);
	    Class ret = PickleClassLoader.loader.loadClass(osc.getName());
	    // System.out.println("Defined: "+ret);
	    // now read the static fields:
	    Field[] fields = ret.getDeclaredFields();
	    int fc = fields.length;
	    for (int i=0; i<fc; i++) {
		int modifier = fields[i].getModifiers();
		if (Modifier.isStatic(modifier)) {
		    Object content = readObject();
		    try {
			fields[i].set(null,content);
		    } catch (IllegalArgumentException I) {
			System.err.println(I);
			I.printStackTrace();
		    } catch (IllegalAccessException I) {
			System.err.println(I);
			I.printStackTrace();
		    }
		}
	    }
	    return ret;
	} else {
	    return super.resolveClass(osc);
	}
    }
}
