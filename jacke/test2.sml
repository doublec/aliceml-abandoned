(* very simple (toplevel) grammar definition *)
structure arithmetic =
struct

    token PLUS | MINUS | TIMES | NUM of int

    assocl TIMES
    assocl PLUS MINUS


    rule exp : int = 
        NUM 
      | n1 as exp, oper, n2 as exp => (oper(n1,n2))
    and oper =
        PLUS  => (op+)
      | MINUS => (op-)
      | TIMES => (op*)

    parser eval = exp
end
