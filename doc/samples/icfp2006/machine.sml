signature MACHINE =
sig
    type machine

    val machine : Store.store -> machine
    val step : machine -> unit
end
