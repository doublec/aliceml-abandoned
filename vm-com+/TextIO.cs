//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Leif Kornstaedt, 2000
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
//

using System;
using System.IO;
using Alice;
using Alice.Values;

class Prebound {
    public static object IO_Io = "IO.Io";
}

class StreamWrapper {
    public string Name;
    public object Stream;
    public StreamWrapper(string name, object stream) {
	Name = name;
	Stream = stream;
    }
}

class TextIO_openIn: Procedure {
    public override object Apply(object a) {
	System.String name = "";
	try {
	    name = (string) CommonOp.Sync(a);
	    FileStream fs =
		new FileStream(name, FileMode.Open, FileAccess.Read);
	    return new StreamWrapper(name, new StreamReader(fs));
	} catch (System.Exception e) {
	    object[] ar = new object[3] {e, "openIn", name}; //--**
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
}

class TextIO_inputAll: Procedure {
    public override object Apply(object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	try {
	    return ((StreamReader) wrapper.Stream).ReadToEnd();
	} catch (System.Exception e) {
	    object[] ar = new object[3] {e, "inputAll", wrapper.Name}; //--**
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
}

class TextIO_inputLine: Procedure {
    static char[] buf = new Char[4097];
    public override object Apply(object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	try {
	    System.String line = ((StreamReader) wrapper.Stream).ReadLine();
	    if (line == null) {
		return "";
	    } else {
		return line + "\n";
	    }
	} catch (System.Exception e) {
	    object[] ar = new object[3] {e, "inputLine", wrapper.Name}; //--**
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
}

class TextIO_closeIn: Procedure {
    public override object Apply(object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	((StreamReader) wrapper.Stream).Close();
	return Alice.Prebound.unit;
    }
}

class TextIO_openOut: Procedure {
    public override object Apply(object a) {
	string name = "";
	try {
	    name = (string) CommonOp.Sync(a);
	    FileStream fs =
		new FileStream(name, FileMode.CreateNew, FileAccess.Write);
	    return new StreamWrapper(name, new StreamWriter(fs));
	} catch (System.Exception e) {
	    object[] ar = new object[3] {e, "openOut", name}; //--**
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
}

class TextIO_output: Procedure2 {
    public override object Apply(object a, object b) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	try {
	    ((StreamWriter) wrapper.Stream).Write((string) CommonOp.Sync(b));
	    return Alice.Prebound.unit;
	} catch (System.Exception e) {
	    object[] ar = new object[3] {e, "output", wrapper.Name}; //--**
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
}

class TextIO_output1: Procedure2 {
    public override object Apply(object a, object b) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	try {
	    ((StreamWriter) wrapper.Stream).Write((Char) CommonOp.Sync(b));
	    return Alice.Prebound.unit;
	} catch (System.Exception e) {
	    object[] ar = new object[3] {e, "output1", wrapper.Name}; //--**
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
}

class TextIO_flushOut: Procedure {
    public override object Apply(object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	((StreamWriter) wrapper.Stream).Flush();
	return Alice.Prebound.unit;
    }
}

class TextIO_closeOut: Procedure {
    public override object Apply(object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	((StreamWriter) wrapper.Stream).Close();
	return Alice.Prebound.unit;
    }
}

class TextIO_print: Procedure {
    public override object Apply(object a) {
	Console.Write((string) CommonOp.Sync(a));
	return Alice.Prebound.unit;
    }
}

public class Execute {
    public static object Main(object obj) {
	object print  = new TextIO_print();
	object stdErr = new StreamWrapper("stderr", System.Console.Error);
	object stdIn  = new StreamWrapper("stdin", System.Console.In);
	object stdOut = new StreamWrapper("stdout", System.Console.Out);

	return new object[2] {
	    new object[17] {             // TextIO$
		null,                    //--** TextIO$.$elem
		null,                    //--** TextIO$.$instream
		null,                    //--** TextIO$.$outstream
		null,                    //--** TextIO$.$vector
		new TextIO_closeIn(),    // TextIO$.closeIn
		new TextIO_closeOut(),   // TextIO$.closeOut
		new TextIO_flushOut(),   // TextIO$.flushOut
		new TextIO_inputAll(),   // TextIO$.inputAll
		new TextIO_inputLine(),  // TextIO$.inputLine
		new TextIO_openIn(),     // TextIO$.openIn
		new TextIO_openOut(),    // TextIO$.openOut
		new TextIO_output(),     // TextIO$.output
		new TextIO_output1(),    // TextIO$.output1
		print,                   // TextIO$.print
		stdErr,                  // TextIO$.stdErr
		stdIn,                   // TextIO$.stdIn
		stdOut                   // TextIO$.stdOut
	    },
	    print                        // print
	};
    }
}
