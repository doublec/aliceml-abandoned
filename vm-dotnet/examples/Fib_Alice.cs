using Alice;
using Alice.Values;
using Alice.Builtins;

public class Fib_Alice {
    static object Module;
    static Fib_Alice() {
	System.Uri baseUri =
	    new System.Uri(System.IO.Directory.GetCurrentDirectory() + "/");
	Komponist.DebugFlag = false;
	Komponist k = new Komponist(baseUri);
	Module = Future_await.StaticApply(k.Import("Fib_Alice_Component"));
    }

    public static string Lang {
	get {
	    return (string)
		CommonOp.Sync(((object[]) CommonOp.Sync(Module))[0]);
	}
    }
    public static int[] NextTerms(int count, int term0, int term1) {
	Procedure nextTerms =
	    (Procedure) CommonOp.Sync(((object[]) CommonOp.Sync(Module))[1]);
	object[] array =
	    (object[]) CommonOp.Sync(nextTerms.Apply(count, term0, term1));

	int[] res = new int[array.Length];
	for (int i = 0; i < array.Length; i++)
	    res[i] = (int) array[i];
	return res;
    }
}
