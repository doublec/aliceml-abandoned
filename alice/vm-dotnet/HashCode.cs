//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last change:
//   $Date$ by $Author$
//   $Revision$
//

using System;
using System.IO;
using System.Text;

public class HashCode {
    public static void Main(string[] args) {
	if (args.Length != 2) {
	    Console.WriteLine("Usage: HashCode <input file> <output file>");
	    Environment.Exit(2);
	}
	StreamReader inFile =
	    new StreamReader(new FileStream(args[0], FileMode.Open,
					    FileAccess.Read));
	StreamWriter outFile =
	    new StreamWriter(new FileStream(args[1], FileMode.Create,
					    FileAccess.Write),
			     new ASCIIEncoding());
	while (true) {
	    string s = inFile.ReadLine();
	    if (s == null)
		break;
	    if (s.StartsWith(".assembly extern ")) {
		outFile.WriteLine(s);
		while (true) {
		    s = inFile.ReadLine();
		    if (s == null)
			break;
		    outFile.WriteLine(s);
		    if (s.Equals("}"))
			break;
		}
	    }
	}
	inFile.Close();
	outFile.Close();
    }
}
