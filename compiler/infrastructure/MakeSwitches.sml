functor MakeSwitches() :> SWITCHES =
  struct
    val printComponentSig		= ref true
    val defaultImport			= ref true
    val outputAssembly			= ref false
  end
