type u = {}
type v = {}
type t = u * v
datatype w

signature S =
    sig
        constructor C of t : w
    end

signature S' =
    sig
        val C : u * v -> w
    end

functor Match(S : S) = S : S'

functor Match(   F : fct(G : fct(X:S) -> S') ->
                      sig
                         functor H : fct(X:S') -> S
                      end
             ) = F : fct(G : fct(X:S') -> S) ->
                      sig
                         functor H : fct(X:S) -> S'
                      end
