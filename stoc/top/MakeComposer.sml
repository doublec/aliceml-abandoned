functor MakeComposer(Sig: SIGNATURE) :> COMPOSER where Sig = Sig =
  struct
    fun loadStat(url)      = raise Crash.Crash "Composer.loadStat" : Sig.t
    fun loadDyn(url, sign) = raise Crash.Crash "Composer.loadDyn" : Value.t

    fun resolve url = raise Crash.Crash "Composer.resolve"
    (* ... *)

    fun sign url  = loadStat(resolve url) (* ... *)
    fun start url = () (* and lazily loadDyn... *)
  end
