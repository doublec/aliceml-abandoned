signature jacke_TOKENS =
sig
type ('a,'b) token
type svalue
val MLKEY: (string) *  'a * 'a -> (svalue,'a) token
val MLOP: (string) *  'a * 'a -> (svalue,'a) token
val DECINTRO: (string) *  'a * 'a -> (svalue,'a) token
val SKIP:  'a * 'a -> (svalue,'a) token
val PREC:  'a * 'a -> (svalue,'a) token
val PARSER:  'a * 'a -> (svalue,'a) token
val RULE:  'a * 'a -> (svalue,'a) token
val NONASSOC:  'a * 'a -> (svalue,'a) token
val ASSOCR:  'a * 'a -> (svalue,'a) token
val ASSOCL:  'a * 'a -> (svalue,'a) token
val TOKEN:  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val ABSTYPE:  'a * 'a -> (svalue,'a) token
val LOCAL:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val MAPSTO:  'a * 'a -> (svalue,'a) token
val AS:  'a * 'a -> (svalue,'a) token
val OF:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val UMINUS:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature jacke_LRVALS=
sig
structure Tokens : jacke_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
