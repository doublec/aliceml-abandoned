signature CONTEXT =
  sig
    type t
    val new :	unit -> t
    val clone :	t -> t
  end
