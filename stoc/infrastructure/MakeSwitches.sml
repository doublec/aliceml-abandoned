functor MakeSwitches() :> SWITCHES =
  struct
    datatype rtt_level = NO_RTT | CORE_RTT | FULL_RTT

    val printComponentSig		= ref true
    val implicitImport			= ref true
    val outputAssembly			= ref false
    val rttLevel			= ref NO_RTT
  end
