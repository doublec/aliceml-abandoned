structure Prebound' :> PREBOUND' =
  struct

    type name   = IntermediateGrammar.stamp

    open IntermediateGrammar

    val dummy = Source.nowhere

    val stamp_builtin	= Stamp.new()
    val stamp_less	= Stamp.new()
    val stamp_plus	= Stamp.new()
    val stamp_times	= Stamp.new()

  end
