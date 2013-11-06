signature S =
  sig
    signature I =
      sig
        type t = {}
        val x : {}
      end

    structure X : I where type t = {}
  end

functor F(B : S) =
  struct
    open B
    val _ = X.x
  end
