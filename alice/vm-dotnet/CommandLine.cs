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
using Alice;
using Alice.Values;

class CommandLine_name: Procedure {
    public override object Apply(object a) {
	return Environment.GetCommandLineArgs()[1];
    }
}

class CommandLine_arguments: Procedure {
    public override object Apply(object a) {
	System.String[] args = Environment.GetCommandLineArgs();
	int len = args.Length;

	if (len == 2) {
	    return 1;
	} else {
	    TagVal head = ListOp.Cons(args[len - 1], 1);
	    for (int i = len - 2; i >= 2; i--) {
		head = (TagVal) ListOp.Cons(args[i], head);
	    }
	    return head;
	}
    }
}

public class Execute {
    public static object Main(object obj) {
	return new object[1] {
	    new object[2] {                   // CommandLine$
		new CommandLine_arguments(),  // CommandLine$.arguments
		new CommandLine_name()        // CommandLine$.name
	    }
	};
    }
}
