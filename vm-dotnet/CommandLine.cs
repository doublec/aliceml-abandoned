using System;
using Alice;
using Alice.Values;

class CommandLine_name : Procedure {
    public static Object StaticApply(Object a) {
	return Environment.GetCommandLineArgs()[1];
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

class CommandLine_arguments : Procedure {
    public static Object StaticApply(Object a) {
	System.String[] args = Environment.GetCommandLineArgs();
	int len              = args.Length;

	if (len == 2) {
	    return (Int32) 1;
	}
	else {
	    TagVal head = ListOp.Cons(args[len - 1], (Int32) 1);
	    for (int i = (len - 2); i >= 2; i--) {
		head = (TagVal) ListOp.Cons(args[i], head);
	    }
	    return head;
	}
    }
    public override Object Apply(Object a) {
	return StaticApply(a);
    }
}

public class Execute {
    public static Object Main(Object obj) {
	Object[] CommandLine = new Object[2];

	CommandLine[0] = new CommandLine_arguments();   // arguments
	CommandLine[1] = new CommandLine_name();        // name

	return CommandLine;
    }
}
