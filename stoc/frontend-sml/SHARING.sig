signature SHARING =
  sig
    type spec      = AbstractGrammar.spec
    type typlongid = AbstractGrammar.typlongid
    type inflongid = AbstractGrammar.inflongid
    type modlongid = AbstractGrammar.modlongid

    val shareTyp :	spec list * typlongid list -> spec list  (* reversed *)
    val shareSig :	spec list * inflongid list -> spec list  (* reversed *)
    val shareStr :	spec list * modlongid list -> spec list  (* reversed *)
  end
