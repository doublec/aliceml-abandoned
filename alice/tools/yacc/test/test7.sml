(* parser for pure lambda calculus *)
structure AbstractSyntax =
struct
    datatype absSyn = 
	Var of string
      | App of absSyn * absSyn
      | Lam of string * absSyn
end

structure Parse =
struct
    structure A = AbstractSyntax

    token VAR of string | LPAR | RPAR | DOT | LAM

    assocl VAR  LPAR LAM 

    rule exp =
      VAR                     => (A.Var VAR)
      | e1 as exp, e2 as exp  => (A.App (e1,e2)) prec VAR
      | LAM, VAR, DOT, exp    => (A.Lam (VAR,exp))
      | LPAR, exp, RPAR       => (exp)

    parser parse = exp 
end
