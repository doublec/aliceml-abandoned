(* Build *)
CM.make();

(* Export *)
SMLofNJ.Internals.GC.messages false;
SMLofNJ.exportFn ("jacke-image", Main.main o ignore);
