using System;
using System.IO;

public class HashCode {
    public static void Main(String[] args) {
	if (args.Length != 2) {
	    Console.WriteLine("Usage: HashCode <input file> <output file>");
	    Environment.Exit(2);
	}
	StreamReader inFile =
	    new StreamReader(new FileStream(args[0], FileMode.Open,
					    FileAccess.Read));
	StreamWriter outFile =
	    new StreamWriter(new FileStream(args[1], FileMode.Create,
					    FileAccess.Write));
	while (true) {
	    String s = inFile.ReadLine();
	    if (s == null)
		break;
	    if (s.StartsWith(".assembly extern ")) {
		outFile.Write(s + "\n");
		while (true) {
		    s = inFile.ReadLine();
		    if (s == null)
			break;
		    outFile.Write(s + "\n");
		    if (s.Equals("}"))
			break;
		}
	    }
	}
	inFile.Close();
	outFile.Close();
    }
}
