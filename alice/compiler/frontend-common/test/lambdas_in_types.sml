datatype 'a option = NONE | SOME of 'a

structure S1 =
    struct
	fun f {} = NONE
    end

signature S =
    sig
	val f: {} -> 'a option
    end

structure S2 :> S = S1

structure X =
    struct
	fun g {} = S2.f {}
    end
