signature I =
    sig
        signature A
        functor F(X : sig
                          signature A = A
                          functor F(X : A) : sig end
                      end) : sig end
    end

signature J =
    sig
        signature A = I
        functor F(X : I) : sig end
    end

(* Try to check J <= I *)

functor Loop(X : J) = X : I
