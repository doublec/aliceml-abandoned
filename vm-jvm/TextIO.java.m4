/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

import java.io.*;

final public class TextIO {
    final public class IStream implements DMLValue {
	InputStream in = null;

	public InStream(InputStream is) {
	    in = is;
	}
	_apply_fails ;
	_request_id ;
	_getValue_id ;
	_raise ;
    }

    final public class OStream implements DMLValue {
	OutputStream out = null;

	public InStream(OutputStream os) {
	    out = os;
	}
	_apply_fails ;
	_request_id ;
	_getValue_id ;
	_raise ;
    }

    /** val stdIn : instream */
    final public static DMLValue stdIn = new IStream(System.in);

    /** val stdOut : instream */
    final public static DMLValue stdOut = new IStream(System.out);

    _BUILTIN(OpenIn) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"TextIO.openIn");
	    DMLValue fn  = args[0].request();
	    if (fn instanceof STRING) {
		java.lang.String filename = ((STRING) fn).getString();
		FileInputStream fs = new FileInputStream(filename);
		return new IStream(fs);
	    } else {
		_error("argument not string",val);
	    }
	}
    }
    /** val openIn : string -> instream */
    _FIELD(TextIO,openIn);

    _BUILTIN(CloseIn) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"TextIO.closeIn");
	    DMLValue fn  = args[0].request();
	    if (fn instanceof IStream) {
		((IStream) fn).in.close();
		return Constants.unit;
	    } else {
		_error("argument not string",val);
	    }
	}
    }
    /** val closeIn : instream -> unit */
    _FIELD(TextIO,closeIn);

    _BUILTIN(InputAll) {
	_APPLY(val) {
	    _fromTulpe(args,val,1,"TextIO.inputAll");
	    DMLValue ins = args[0].request();
	    if (ins instanceof IStream) {
		InputStream in = ((IStream) ins).in;
		int len = in.available();
		byte[] b = new byte[len];
		in.read(b,0,len);
		return STRING (java.lang.String(b));
	    } else {
		_error("argument not instream");
	    }
	}
    }
    /** val inputAll : instream -> string */
    _FIELD(TextIO,inputAll);
}
