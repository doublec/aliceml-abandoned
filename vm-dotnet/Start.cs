using System;

class Start {
    public static void Main(System.String[] args) {
	Alice.Komponist k = new Alice.Komponist();
	Alice.Komponist.global_k = k;
	
	íf (args.Length < 2) {
	    Console.Write("usage: ");
	    Console.Write(args[0]);
	    Console.WriteLine(" dll progargs");
	}
	else {
	    k.Import(args[1]);
	    // (AliceFuture) k.Import(args[i])).Await();
	}
    }
}
