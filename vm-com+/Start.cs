using System;

class Start {
    public static void Main(System.String[] args) {
	Alice.Komponist k = new Alice.Komponist();
	Alice.Komponist.global_k = k;
	
	if (args.Length < 2) {
	    Console.Write("usage: ");
	    Console.Write(args[0]);
	    Console.WriteLine(" dll progargs");
	}
	else {
	    Alice.Builtins.Future_await.StaticApply(k.Import(args[1]));
	}
    }
}
