(* abstract syntax for parse tree of jacke input *)
structure AbsSyn =
struct
    datatype bnfexp =
	Skip
      | Symbol of string
      | As of string * bnfexp
      | Seq of bnfexp list
      | Prec of bnfexp * string
      | Transform of bnfexp * string list
      | Alt of bnfexp list
	
    and parsetree =
	TokenDec of (string * string option) list
  | AssoclDec of string list
      | AssocrDec of string list
      | NonassocDec of string list
      | RuleDec of rule list
      | ParserDec of (string * string option * string) list
      | MLCode of string list
	
    withtype 
	rule = string * string option * bnfexp
end
