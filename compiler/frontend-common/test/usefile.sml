SMLofNJ.Internals.GC.messages false;
Compiler.Control.Print.printDepth := 1000;
val _ = CM.make()
fun m _ = CM.make()
fun e _ = ignore(Main.elabFile f)
