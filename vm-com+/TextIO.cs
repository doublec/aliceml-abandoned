using System;
using System.IO;
using Alice;
using Alice.Values;

class Prebound {
    public static Object IO_Io = "IO.Io";
}

class StreamWrapper {
    public System.String name;
    public Object stream;
    public StreamWrapper(System.String name, Object stream) {
	this.name   = name;
	this.stream = stream;
    }
}

class TextIO_openIn : Procedure {
    public static Object StaticApply(Object a) {
	System.String name = "";
	try {
	    name = (System.String) CommonOp.Sync(a);
	    return new StreamWrapper(name,
				     new StreamReader(new FileStream(name,
								     FileMode.Open,
								     FileAccess.Read)));
	}
	catch (System.Exception e) {
	    Object[] ar = new Object[3];
	    ar[0]       = e; // to be determined
	    ar[1]       = "openIn";
	    ar[2]       = name;
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class TextIO_inputAll : Procedure {
    public static Object StaticApply(Object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	StreamReader r        = (StreamReader) wrapper.stream;
	try {
	    return r.ReadToEnd();
	}
	catch (System.Exception e) {
	    Object[] ar = new Object[3];
	    ar[0] = e; // to be determined
	    ar[1] = "inputAll";
	    ar[2] = wrapper.name;
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class TextIO_inputLine : Procedure {
    static Char[] buf = new Char[4097];
    public static Object StaticApply(Object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	StreamReader r        = (StreamReader) wrapper.stream;
	try {
	    System.String line = r.ReadLine();

	    if (line == "") {
		return line;
	    }
	    else {
		// SML Base Library helper text
		// if the file ends without newline, stick it on anyway
		return System.String.Concat(line, "\n");
	    }
	}
	catch (System.Exception e) {
	    Object[] ar = new Object[3];
	    ar[0]       = e; // to be determined
	    ar[1]       = "inputLine";
	    ar[2]       = wrapper.name;
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class TextIO_closeIn : Procedure {
    public static Object StaticApply(Object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	StreamReader r        = (StreamReader) wrapper.stream;

	r.Close();
	return Alice.Prebound.unit;
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class TextIO_openOut : Procedure {
    public static Object StaticApply(Object a) {
	System.String name = "";
	try {
	    name = (System.String) CommonOp.Sync(a);
	    return new StreamWrapper(name,
				     new StreamWriter(new FileStream(name,
								     FileMode.CreateNew,
								     FileAccess.Write)));
	}
	catch (System.Exception e) {
	    Object[] ar = new Object[3];
	    ar[0]       = e; // to be determined
	    ar[1]       = "openOut";
	    ar[2]       = name;
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class TextIO_output : Procedure2 {
    public static Object StaticApply(Object a, Object b) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	StreamWriter  w       = (StreamWriter) wrapper.stream;
	try {
	    w.Write((System.String) CommonOp.Sync(b));
	    return Alice.Prebound.unit;
	}
	catch (System.Exception e) {
	    Object[] ar = new Object[3];
	    ar[0] = e; // to be determined
	    ar[1] = "output";
	    ar[2] = wrapper.name;
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
    public override Object Apply(Object a, Object b) {
	return StaticApply(a, b);
    }
}

class TextIO_output1 : Procedure2 {
    public static Object StaticApply(Object a, Object b) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	StreamWriter  w       = (StreamWriter) wrapper.stream;
	try {
	    w.Write((System.Char) CommonOp.Sync(b));
	    return Alice.Prebound.unit;
	}
	catch (System.Exception e) {
	    Object[] ar = new Object[3];
	    ar[0] = e; // to be determined
	    ar[1] = "output1";
	    ar[2] = wrapper.name;
	    throw new Alice.Values.Exception(new ConVal(Prebound.IO_Io, ar));
	}
    }
    public override Object Apply(Object a, Object b) {
	return StaticApply(a, b);
    }
}

class TextIO_flushOut : Procedure {
    public static Object StaticApply(Object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	StreamWriter w        = (StreamWriter) wrapper.stream;

	w.Flush();
	return Alice.Prebound.unit;
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class TextIO_closeOut : Procedure {
    public static Object StaticApply(Object a) {
	StreamWrapper wrapper = (StreamWrapper) CommonOp.Sync(a);
	StreamWriter w        = (StreamWriter) wrapper.stream;

	w.Close();
	return Alice.Prebound.unit;
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class TextIO_print : Procedure {
    public static Object StaticApply(Object a) {
	Console.Write((System.String) CommonOp.Sync(a));
	return Alice.Prebound.unit;
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

public class Execute {
    public static Object Main(Object obj) {
	Object[] TextIO = new Object[13];

	TextIO[0] = new TextIO_closeIn();                        // closeIn
	TextIO[1] = new TextIO_closeOut();                       // closeOut
	TextIO[2] = new TextIO_flushOut();                       // flushOut
	TextIO[3] = new TextIO_inputAll();                       // inputAll
	TextIO[4] = new TextIO_inputLine();                      // inputLine
	TextIO[5] = new TextIO_openIn();                         // openIn
	TextIO[6] = new TextIO_openOut();                        // openOut
	TextIO[7] = new TextIO_output();                         // output
	TextIO[8] = new TextIO_output1();                        // output1
	TextIO[9] = new TextIO_print();                          // print
	TextIO[10] =
	    new StreamWrapper("stderr", System.Console.Error);   // stdErr
	TextIO[11] =
	    new StreamWrapper("stdin", System.Console.In);       // stdIn
	TextIO[12] =
	    new StreamWrapper("stdout", System.Console.Out);     // stdOut

	return TextIO;
    }
}
