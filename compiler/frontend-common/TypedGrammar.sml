structure TypedInfo =
  struct
    datatype annotation = NON | TYP of Type.t | INF of unit (*UNFINISHED*)
    type     info       = Source.position * annotation
  end

structure TypedGrammar = MakeAbstractGrammar(TypedInfo)
