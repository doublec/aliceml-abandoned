using System;

class Start {
    public static void Main(System.String[] args) {
	Alice.Komponist k = new Alice.Komponist();
	
	Alice.Komponist.global_k = k;
	for (int i = 0 ; i < args.Length; i++) {
	    k.Import(args[i]);
	    // (AliceFuture) k.Import(args[i])).Await();
	}
    }
}
