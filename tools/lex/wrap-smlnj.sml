(* Build *)
CM.make();

(* Export *)
SMLofNJ.Internals.GC.messages false;
SMLofNJ.exportFn ("hose-image", Hose.main o ignore);
