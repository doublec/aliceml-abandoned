structure TypedInfo =
  struct
    datatype annotation = NON | TYP of Type.t | INF of Inf.t
    type     info       = Source.region * annotation
  end

structure TypedGrammar = MakeAbstractGrammar(TypedInfo)
