public class Test {
    public static void Main(string[] args) {
	System.Console.WriteLine(Fib_Alice.Lang);
	int[] array = Fib_Alice.NextTerms(5, 1, 1);
	for (int i = 0; i < 5; i++)
	    System.Console.WriteLine(array[i]);
    }
}
