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

final public class TextIO {
    final public static Name IO = new UniqueName("TextIO.IO");

    final public static class IStream implements DMLValue {
	InputStream in = null;

	public IStream(InputStream is) {
	    in = is;
	}
	_apply_fails ;
	_nomatch;
    }

    final public static class OStream implements DMLValue {
	OutputStream out = null;

	public OStream(OutputStream os) {
	    out = os;
	}
	_apply_fails ;
	_nomatch;
    }

    /** val stdIn : instream */
    final public static DMLValue stdIn = new IStream(System.in);
    static {
	Builtin.builtins.put("TextIO.stdIn",stdIn);
    }

    /** val stdOut : outstream */
    final public static DMLValue stdOut = new OStream(System.out);
    static {
	Builtin.builtins.put("TextIO.stdOut",stdOut);
    }

    /** val stdErr : outstream */
    final public static DMLValue stdErr = new OStream(System.err);
    static {
	Builtin.builtins.put("TextIO.stdErr",stdErr);
    }

    _BUILTIN(OpenIn) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"TextIO.openIn");
	    if (val instanceof STRING) {
		java.lang.String filename = ((STRING) val).value;
		FileInputStream fs = null;
		try {
		     fs = new FileInputStream(filename);
		     return new IStream(fs);
		} catch (FileNotFoundException f) {
		    System.err.println(f);
		    f.printStackTrace();
		    _RAISENAME(IO);
		}
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** val openIn : string -> instream */
    _FIELD(TextIO,openIn);

    _BUILTIN(CloseIn) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"TextIO.closeIn");
	    if (val instanceof IStream) {
		try {
		    ((IStream) val).in.close();
		    return Constants.dmlunit;
		} catch (IOException i) {
		    System.err.println(i);
		    i.printStackTrace();
		    _RAISENAME(IO);
		}
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** val closeIn : instream -> unit */
    _FIELD(TextIO,closeIn);

    _BUILTIN(InputAll) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"TextIO.inputAll");
	    if (val instanceof IStream) {
		try {
		    InputStream in = ((IStream) val).in;
		    int len = in.available();
		    byte[] b = new byte[len];
		    in.read(b,0,len);
		    return new STRING (new java.lang.String(b));
		} catch (IOException i) {
		    System.err.println(i);
		    i.printStackTrace();
		    _RAISENAME(IO);
		}
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** val inputAll : instream -> string */
    _FIELD(TextIO,inputAll);
}
