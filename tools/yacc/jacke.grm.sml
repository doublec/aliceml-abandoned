functor jackeLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : jacke_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* grammar for jacke's own parser *)

structure A = AbsSyn
datatype ('a,'b) sum = In1 of 'a | In2 of 'b

fun lookup "bogus" = 10000
  | lookup s = 0


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\060\000\
\\010\000\022\000\012\000\021\000\014\000\020\000\015\000\019\000\
\\022\000\018\000\023\000\017\000\025\000\016\000\026\000\015\000\
\\038\000\008\000\039\000\007\000\040\000\006\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\009\000\089\000\
\\010\000\022\000\012\000\021\000\014\000\020\000\015\000\019\000\
\\022\000\018\000\023\000\017\000\025\000\016\000\026\000\015\000\
\\038\000\008\000\039\000\007\000\040\000\006\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\010\000\022\000\
\\011\000\059\000\012\000\021\000\014\000\020\000\015\000\019\000\
\\022\000\018\000\023\000\017\000\025\000\016\000\026\000\015\000\
\\038\000\008\000\039\000\007\000\040\000\006\000\000\000\
\\001\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\010\000\022\000\
\\012\000\021\000\013\000\058\000\014\000\020\000\015\000\019\000\
\\022\000\018\000\023\000\017\000\025\000\016\000\026\000\015\000\
\\038\000\008\000\039\000\007\000\040\000\006\000\000\000\
\\001\000\002\000\034\000\000\000\
\\001\000\002\000\037\000\000\000\
\\001\000\002\000\039\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\002\000\062\000\000\000\
\\001\000\002\000\063\000\000\000\
\\001\000\002\000\068\000\008\000\067\000\037\000\066\000\000\000\
\\001\000\002\000\069\000\000\000\
\\001\000\002\000\071\000\000\000\
\\001\000\002\000\080\000\000\000\
\\001\000\002\000\081\000\000\000\
\\001\000\005\000\076\000\009\000\085\000\025\000\075\000\026\000\074\000\
\\036\000\073\000\000\000\
\\001\000\006\000\050\000\015\000\049\000\000\000\
\\001\000\006\000\053\000\015\000\052\000\000\000\
\\001\000\008\000\083\000\000\000\
\\001\000\015\000\072\000\000\000\
\\001\000\015\000\079\000\000\000\
\\091\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\010\000\022\000\
\\012\000\021\000\014\000\020\000\015\000\019\000\022\000\018\000\
\\023\000\017\000\025\000\016\000\026\000\015\000\030\000\014\000\
\\031\000\013\000\032\000\012\000\033\000\011\000\034\000\010\000\
\\035\000\009\000\038\000\008\000\039\000\007\000\040\000\006\000\000\000\
\\092\000\002\000\029\000\003\000\028\000\004\000\027\000\005\000\026\000\
\\006\000\025\000\007\000\024\000\008\000\023\000\010\000\022\000\
\\012\000\021\000\014\000\020\000\015\000\019\000\022\000\018\000\
\\023\000\017\000\025\000\016\000\026\000\015\000\030\000\014\000\
\\031\000\013\000\032\000\012\000\033\000\011\000\034\000\010\000\
\\035\000\009\000\038\000\008\000\039\000\007\000\040\000\006\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\022\000\051\000\000\000\
\\102\000\022\000\048\000\000\000\
\\103\000\026\000\055\000\000\000\
\\104\000\000\000\
\\105\000\023\000\056\000\000\000\
\\106\000\000\000\
\\107\000\002\000\039\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\005\000\076\000\025\000\075\000\026\000\074\000\036\000\073\000\000\000\
\\112\000\005\000\076\000\025\000\075\000\026\000\074\000\036\000\073\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\024\000\078\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\005\000\076\000\025\000\075\000\036\000\073\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\"
val actionRowNumbers =
"\023\000\022\000\025\000\024\000\
\\071\000\072\000\075\000\005\000\
\\006\000\007\000\007\000\007\000\
\\008\000\070\000\074\000\069\000\
\\068\000\064\000\073\000\056\000\
\\056\000\056\000\067\000\066\000\
\\065\000\060\000\061\000\063\000\
\\027\000\026\000\044\000\033\000\
\\017\000\040\000\032\000\018\000\
\\031\000\038\000\030\000\029\000\
\\034\000\028\000\036\000\004\000\
\\003\000\001\000\005\000\009\000\
\\010\000\006\000\011\000\012\000\
\\039\000\008\000\013\000\057\000\
\\059\000\062\000\058\000\045\000\
\\046\000\020\000\041\000\042\000\
\\048\000\011\000\049\000\021\000\
\\035\000\037\000\014\000\015\000\
\\011\000\019\000\011\000\016\000\
\\011\000\011\000\047\000\053\000\
\\055\000\056\000\052\000\050\000\
\\051\000\043\000\002\000\054\000\
\\000\000"
val gotoT =
"\
\\001\000\088\000\002\000\003\000\011\000\002\000\013\000\001\000\000\000\
\\002\000\029\000\011\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\031\000\009\000\030\000\000\000\
\\006\000\034\000\007\000\033\000\000\000\
\\005\000\036\000\000\000\
\\005\000\038\000\000\000\
\\005\000\039\000\000\000\
\\003\000\041\000\004\000\040\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\043\000\000\000\
\\012\000\044\000\000\000\
\\012\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\052\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\055\000\000\000\
\\011\000\055\000\000\000\
\\011\000\055\000\000\000\
\\009\000\059\000\000\000\
\\000\000\
\\000\000\
\\007\000\062\000\000\000\
\\010\000\063\000\000\000\
\\000\000\
\\000\000\
\\003\000\068\000\004\000\040\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\075\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\080\000\000\000\
\\000\000\
\\010\000\082\000\000\000\
\\000\000\
\\010\000\084\000\000\000\
\\010\000\085\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\086\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\055\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 89
val numrules = 54
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | MLKEY of unit ->  (string) | MLOP of unit ->  (string)
 | DECINTRO of unit ->  (string) | STRING of unit ->  (string)
 | INT of unit ->  (int) | ID of unit ->  (string)
 | program of unit ->  (A.parsetreeWithPos list)
 | explist of unit ->  (string list) | exp of unit ->  (string list)
 | bnfexp of unit ->  (A.bnfexpWithPos)
 | parsbind of unit ->  (string*string option*string)
 | parsbinds of unit ->  ( ( string*string option * string )  list)
 | rulebind of unit ->  (A.Prule)
 | rulebinds of unit ->  (A.Prule list)
 | idlist of unit ->  (string list)
 | tokbind of unit ->  (string*string option)
 | tokbinds of unit ->  ( ( string * string option )  list)
 | jackedec of unit ->  (A.parsetreeWithPos)
 | start of unit ->  (A.parsetreeWithPos list)
end
type svalue = MlyValue.svalue
type result = A.parsetreeWithPos list
end
structure EC=
struct
open LrTable
val is_keyword =
fn (T 18) => true | (T 26) => true | (T 19) => true | (T 20) => true
 | (T 15) => true | (T 16) => true | (T 17) => true | (T 21) => true
 | (T 29) => true | (T 22) => true | (T 23) => true | (T 30) => true
 | (T 31) => true | (T 32) => true | (T 33) => true | (T 34) => true
 | (T 35) => true | (T 36) => true | _ => false
val preferred_change = 
(nil
,(T 16) :: nil
)::
(nil
,(T 17) :: nil
)::
(nil
,(T 7) :: nil
)::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "UMINUS"
  | (T 14) => "EQ"
  | (T 15) => "IF"
  | (T 16) => "THEN"
  | (T 17) => "ELSE"
  | (T 18) => "LET"
  | (T 19) => "IN"
  | (T 20) => "END"
  | (T 21) => "AND"
  | (T 22) => "OF"
  | (T 23) => "AS"
  | (T 24) => "MAPSTO"
  | (T 25) => "BAR"
  | (T 26) => "LOCAL"
  | (T 27) => "ABSTYPE"
  | (T 28) => "WITH"
  | (T 29) => "TOKEN"
  | (T 30) => "ASSOCL"
  | (T 31) => "ASSOCR"
  | (T 32) => "NONASSOC"
  | (T 33) => "RULE"
  | (T 34) => "PARSER"
  | (T 35) => "PREC"
  | (T 36) => "SKIP"
  | (T 37) => "DECINTRO"
  | (T 38) => "MLOP"
  | (T 39) => "MLKEY"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms = (T 0) :: (T 4) :: (T 5) :: (T 6) :: (T 7) :: (T 8) :: (T 9
) :: (T 10) :: (T 11) :: (T 12) :: (T 13) :: (T 14) :: (T 15) :: (T 16
) :: (T 17) :: (T 18) :: (T 19) :: (T 20) :: (T 21) :: (T 22) :: (T 23
) :: (T 24) :: (T 25) :: (T 26) :: (T 27) :: (T 28) :: (T 29) :: (T 30
) :: (T 31) :: (T 32) :: (T 33) :: (T 34) :: (T 35) :: (T 36) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.program program1,program1left,program1right))::
rest671) => let val result=MlyValue.start(fn _ => let val program as 
program1=program1 ()
 in (rev program) end
)
 in (LrTable.NT 0,(result,program1left,program1right),rest671) end
| (1,rest671) => let val result=MlyValue.start(fn _ => ([]))
 in (LrTable.NT 0,(result,defaultPos,defaultPos),rest671) end
| (2,(_,(MlyValue.jackedec jackedec1,jackedec1left,jackedec1right))::
rest671) => let val result=MlyValue.program(fn _ => let val jackedec
 as jackedec1=jackedec1 ()
 in ([jackedec]) end
)
 in (LrTable.NT 12,(result,jackedec1left,jackedec1right),rest671) end
| (3,(_,(MlyValue.exp exp1,expleft as exp1left,expright as exp1right))
::rest671) => let val result=MlyValue.program(fn _ => let val exp as 
exp1=exp1 ()
 in ([A.PMLCode (exp,expleft,expright)]) end
)
 in (LrTable.NT 12,(result,exp1left,exp1right),rest671) end
| (4,(_,(MlyValue.jackedec jackedec1,_,jackedec1right))::(_,(
MlyValue.program program1,program1left,_))::rest671) => let val result
=MlyValue.program(fn _ => let val program as program1=program1 ()
val jackedec as jackedec1=jackedec1 ()
 in (jackedec::program) end
)
 in (LrTable.NT 12,(result,program1left,jackedec1right),rest671) end
| (5,(_,(MlyValue.exp exp1,expleft,expright as exp1right))::(_,(
MlyValue.program program1,program1left,_))::rest671) => let val result
=MlyValue.program(fn _ => let val program as program1=program1 ()
val exp as exp1=exp1 ()
 in ((A.PMLCode (exp,expleft,expright))::program) end
)
 in (LrTable.NT 12,(result,program1left,exp1right),rest671) end
| (6,(_,(MlyValue.tokbinds tokbinds1,_,tokbindsright as tokbinds1right
))::(_,(_,TOKENleft as TOKEN1left,_))::rest671) => let val result=
MlyValue.jackedec(fn _ => let val tokbinds as tokbinds1=tokbinds1 ()
 in (A.PTokenDec(tokbinds,TOKENleft,tokbindsright)) end
)
 in (LrTable.NT 1,(result,TOKEN1left,tokbinds1right),rest671) end
| (7,(_,(MlyValue.idlist idlist1,_,idlistright as idlist1right))::(_,(
_,ASSOCLleft as ASSOCL1left,_))::rest671) => let val result=
MlyValue.jackedec(fn _ => let val idlist as idlist1=idlist1 ()
 in (A.PAssoclDec(idlist,ASSOCLleft,idlistright)) end
)
 in (LrTable.NT 1,(result,ASSOCL1left,idlist1right),rest671) end
| (8,(_,(MlyValue.idlist idlist1,_,idlistright as idlist1right))::(_,(
_,ASSOCRleft as ASSOCR1left,_))::rest671) => let val result=
MlyValue.jackedec(fn _ => let val idlist as idlist1=idlist1 ()
 in (A.PAssocrDec(idlist,ASSOCRleft,idlistright)) end
)
 in (LrTable.NT 1,(result,ASSOCR1left,idlist1right),rest671) end
| (9,(_,(MlyValue.idlist idlist1,_,idlistright as idlist1right))::(_,(
_,NONASSOCleft as NONASSOC1left,_))::rest671) => let val result=
MlyValue.jackedec(fn _ => let val idlist as idlist1=idlist1 ()
 in (A.PNonassocDec(idlist,NONASSOCleft,idlistright)) end
)
 in (LrTable.NT 1,(result,NONASSOC1left,idlist1right),rest671) end
| (10,(_,(MlyValue.rulebinds rulebinds1,_,rulebindsright as 
rulebinds1right))::(_,(_,RULEleft as RULE1left,_))::rest671) => let 
val result=MlyValue.jackedec(fn _ => let val rulebinds as rulebinds1=
rulebinds1 ()
 in (A.PRuleDec(rev rulebinds,RULEleft,rulebindsright)) end
)
 in (LrTable.NT 1,(result,RULE1left,rulebinds1right),rest671) end
| (11,(_,(MlyValue.parsbinds parsbinds1,_,parsbindsright as 
parsbinds1right))::(_,(_,PARSERleft as PARSER1left,_))::rest671) => 
let val result=MlyValue.jackedec(fn _ => let val parsbinds as 
parsbinds1=parsbinds1 ()
 in (A.PParserDec(parsbinds,PARSERleft,parsbindsright)) end
)
 in (LrTable.NT 1,(result,PARSER1left,parsbinds1right),rest671) end
| (12,(_,(MlyValue.tokbind tokbind1,tokbind1left,tokbind1right))::
rest671) => let val result=MlyValue.tokbinds(fn _ => let val tokbind
 as tokbind1=tokbind1 ()
 in ([tokbind]) end
)
 in (LrTable.NT 2,(result,tokbind1left,tokbind1right),rest671) end
| (13,(_,(MlyValue.tokbinds tokbinds1,_,tokbinds1right))::_::(_,(
MlyValue.tokbind tokbind1,tokbind1left,_))::rest671) => let val result
=MlyValue.tokbinds(fn _ => let val tokbind as tokbind1=tokbind1 ()
val tokbinds as tokbinds1=tokbinds1 ()
 in (tokbind::tokbinds) end
)
 in (LrTable.NT 2,(result,tokbind1left,tokbinds1right),rest671) end
| (14,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.tokbind(fn _ => let val ID as ID1=ID1 ()
 in ((ID,NONE)) end
)
 in (LrTable.NT 3,(result,ID1left,ID1right),rest671) end
| (15,(_,(MlyValue.ID ID2,_,ID2right))::_::(_,(MlyValue.ID ID1,ID1left
,_))::rest671) => let val result=MlyValue.tokbind(fn _ => let val ID1=
ID1 ()
val ID2=ID2 ()
 in ((ID1,SOME ID2)) end
)
 in (LrTable.NT 3,(result,ID1left,ID2right),rest671) end
| (16,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.idlist(fn _ => let val ID as ID1=ID1 ()
 in ([ID]) end
)
 in (LrTable.NT 4,(result,ID1left,ID1right),rest671) end
| (17,(_,(MlyValue.idlist idlist1,_,idlist1right))::(_,(MlyValue.ID 
ID1,ID1left,_))::rest671) => let val result=MlyValue.idlist(fn _ => 
let val ID as ID1=ID1 ()
val idlist as idlist1=idlist1 ()
 in (ID::idlist) end
)
 in (LrTable.NT 4,(result,ID1left,idlist1right),rest671) end
| (18,(_,(MlyValue.rulebind rulebind1,rulebind1left,rulebind1right))::
rest671) => let val result=MlyValue.rulebinds(fn _ => let val rulebind
 as rulebind1=rulebind1 ()
 in ([rulebind]) end
)
 in (LrTable.NT 5,(result,rulebind1left,rulebind1right),rest671) end
| (19,(_,(MlyValue.rulebind rulebind1,_,rulebind1right))::_::(_,(
MlyValue.rulebinds rulebinds1,rulebinds1left,_))::rest671) => let val 
result=MlyValue.rulebinds(fn _ => let val rulebinds as rulebinds1=
rulebinds1 ()
val rulebind as rulebind1=rulebind1 ()
 in (rulebind::rulebinds) end
)
 in (LrTable.NT 5,(result,rulebinds1left,rulebind1right),rest671) end
| (20,(_,(MlyValue.bnfexp bnfexp1,_,bnfexp1right))::_::(_,(MlyValue.ID
 ID1,ID1left,_))::rest671) => let val result=MlyValue.rulebind(fn _
 => let val ID as ID1=ID1 ()
val bnfexp as bnfexp1=bnfexp1 ()
 in ((ID,NONE,bnfexp)) end
)
 in (LrTable.NT 6,(result,ID1left,bnfexp1right),rest671) end
| (21,(_,(MlyValue.bnfexp bnfexp1,_,bnfexp1right))::_::(_,(MlyValue.ID
 ID2,_,_))::_::(_,(MlyValue.ID ID1,ID1left,_))::rest671) => let val 
result=MlyValue.rulebind(fn _ => let val ID1=ID1 ()
val ID2=ID2 ()
val bnfexp as bnfexp1=bnfexp1 ()
 in ((ID1,SOME ID2,bnfexp)) end
)
 in (LrTable.NT 6,(result,ID1left,bnfexp1right),rest671) end
| (22,(_,(MlyValue.parsbind parsbind1,parsbind1left,parsbind1right))::
rest671) => let val result=MlyValue.parsbinds(fn _ => let val parsbind
 as parsbind1=parsbind1 ()
 in ([parsbind]) end
)
 in (LrTable.NT 7,(result,parsbind1left,parsbind1right),rest671) end
| (23,(_,(MlyValue.parsbind parsbind1,_,parsbind1right))::_::(_,(
MlyValue.parsbinds parsbinds1,parsbinds1left,_))::rest671) => let val 
result=MlyValue.parsbinds(fn _ => let val parsbinds as parsbinds1=
parsbinds1 ()
val parsbind as parsbind1=parsbind1 ()
 in (parsbind::parsbinds) end
)
 in (LrTable.NT 7,(result,parsbinds1left,parsbind1right),rest671) end
| (24,(_,(MlyValue.ID ID2,_,ID2right))::_::(_,(MlyValue.ID ID1,ID1left
,_))::rest671) => let val result=MlyValue.parsbind(fn _ => let val ID1
=ID1 ()
val ID2=ID2 ()
 in ((ID1,NONE,ID2)) end
)
 in (LrTable.NT 8,(result,ID1left,ID2right),rest671) end
| (25,(_,(MlyValue.ID ID3,_,ID3right))::_::(_,(MlyValue.ID ID2,_,_))::
_::(_,(MlyValue.ID ID1,ID1left,_))::rest671) => let val result=
MlyValue.parsbind(fn _ => let val ID1=ID1 ()
val ID2=ID2 ()
val ID3=ID3 ()
 in ((ID1,SOME ID2,ID3)) end
)
 in (LrTable.NT 8,(result,ID1left,ID3right),rest671) end
| (26,(_,(_,SKIP1left,SKIP1right))::rest671) => let val result=
MlyValue.bnfexp(fn _ => (A.PSkip))
 in (LrTable.NT 9,(result,SKIP1left,SKIP1right),rest671) end
| (27,(_,(MlyValue.ID ID1,IDleft as ID1left,IDright as ID1right))::
rest671) => let val result=MlyValue.bnfexp(fn _ => let val ID as ID1=
ID1 ()
 in (A.PSymbol(ID,IDleft,IDright)) end
)
 in (LrTable.NT 9,(result,ID1left,ID1right),rest671) end
| (28,(_,(_,_,RPAREN1right))::(_,(MlyValue.bnfexp bnfexp1,_,_))::(_,(_
,LPAREN1left,_))::rest671) => let val result=MlyValue.bnfexp(fn _ => 
let val bnfexp as bnfexp1=bnfexp1 ()
 in (bnfexp) end
)
 in (LrTable.NT 9,(result,LPAREN1left,RPAREN1right),rest671) end
| (29,(_,(MlyValue.bnfexp bnfexp1,_,bnfexpright as bnfexp1right))::_::
(_,(MlyValue.ID ID1,IDleft as ID1left,_))::rest671) => let val result=
MlyValue.bnfexp(fn _ => let val ID as ID1=ID1 ()
val bnfexp as bnfexp1=bnfexp1 ()
 in (A.PAs(ID,bnfexp,IDleft,bnfexpright)) end
)
 in (LrTable.NT 9,(result,ID1left,bnfexp1right),rest671) end
| (30,(_,(MlyValue.bnfexp bnfexp2,_,bnfexp2right))::_::(_,(
MlyValue.bnfexp bnfexp1,bnfexp1left,_))::rest671) => let val result=
MlyValue.bnfexp(fn _ => let val bnfexp1=bnfexp1 ()
val bnfexp2=bnfexp2 ()
 in (A.PSeq([bnfexp1,bnfexp2],bnfexp1left,bnfexp2right)) end
)
 in (LrTable.NT 9,(result,bnfexp1left,bnfexp2right),rest671) end
| (31,(_,(MlyValue.ID ID1,_,IDright as ID1right))::_::(_,(
MlyValue.bnfexp bnfexp1,bnfexpleft as bnfexp1left,_))::rest671) => 
let val result=MlyValue.bnfexp(fn _ => let val bnfexp as bnfexp1=
bnfexp1 ()
val ID as ID1=ID1 ()
 in (A.PPrec(bnfexp,ID,bnfexpleft,IDright)) end
)
 in (LrTable.NT 9,(result,bnfexp1left,ID1right),rest671) end
| (32,(_,(_,_,RPARENright as RPAREN1right))::(_,(MlyValue.explist 
explist1,_,_))::_::_::(_,(MlyValue.bnfexp bnfexp1,bnfexpleft as 
bnfexp1left,_))::rest671) => let val result=MlyValue.bnfexp(fn _ => 
let val bnfexp as bnfexp1=bnfexp1 ()
val explist as explist1=explist1 ()
 in (A.PTransform(bnfexp,explist,bnfexpleft,RPARENright)) end
)
 in (LrTable.NT 9,(result,bnfexp1left,RPAREN1right),rest671) end
| (33,(_,(MlyValue.bnfexp bnfexp2,_,bnfexp2right))::_::(_,(
MlyValue.bnfexp bnfexp1,bnfexp1left,_))::rest671) => let val result=
MlyValue.bnfexp(fn _ => let val bnfexp1=bnfexp1 ()
val bnfexp2=bnfexp2 ()
 in (A.PAlt([bnfexp1,bnfexp2],bnfexp1left,bnfexp2right)) end
)
 in (LrTable.NT 9,(result,bnfexp1left,bnfexp2right),rest671) end
| (34,rest671) => let val result=MlyValue.explist(fn _ => ([]))
 in (LrTable.NT 11,(result,defaultPos,defaultPos),rest671) end
| (35,(_,(MlyValue.exp exp1,_,exp1right))::(_,(MlyValue.explist 
explist1,explist1left,_))::rest671) => let val result=MlyValue.explist
(fn _ => let val explist as explist1=explist1 ()
val exp as exp1=exp1 ()
 in (explist@exp) end
)
 in (LrTable.NT 11,(result,explist1left,exp1right),rest671) end
| (36,(_,(_,_,RPAREN1right))::(_,(MlyValue.explist explist1,_,_))::(_,
(_,LPAREN1left,_))::rest671) => let val result=MlyValue.exp(fn _ => 
let val explist as explist1=explist1 ()
 in ("("::(explist@[")"])) end
)
 in (LrTable.NT 10,(result,LPAREN1left,RPAREN1right),rest671) end
| (37,(_,(_,_,RBRACE1right))::(_,(MlyValue.explist explist1,_,_))::(_,
(_,LBRACE1left,_))::rest671) => let val result=MlyValue.exp(fn _ => 
let val explist as explist1=explist1 ()
 in ("{"::(explist@["}"])) end
)
 in (LrTable.NT 10,(result,LBRACE1left,RBRACE1right),rest671) end
| (38,(_,(MlyValue.STRING STRING1,STRING1left,STRING1right))::rest671)
 => let val result=MlyValue.exp(fn _ => let val STRING as STRING1=
STRING1 ()
 in (["\""^STRING^"\""]) end
)
 in (LrTable.NT 10,(result,STRING1left,STRING1right),rest671) end
| (39,(_,(MlyValue.INT INT1,INT1left,INT1right))::rest671) => let val 
result=MlyValue.exp(fn _ => let val INT as INT1=INT1 ()
 in ([Int.toString INT]) end
)
 in (LrTable.NT 10,(result,INT1left,INT1right),rest671) end
| (40,(_,(_,_,RBRACK1right))::(_,(MlyValue.explist explist1,_,_))::(_,
(_,LBRACK1left,_))::rest671) => let val result=MlyValue.exp(fn _ => 
let val explist as explist1=explist1 ()
 in ("["::(explist@["]"])) end
)
 in (LrTable.NT 10,(result,LBRACK1left,RBRACK1right),rest671) end
| (41,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.exp(fn _ => let val ID as ID1=ID1 ()
 in ([ID]) end
)
 in (LrTable.NT 10,(result,ID1left,ID1right),rest671) end
| (42,(_,(_,EQ1left,EQ1right))::rest671) => let val result=
MlyValue.exp(fn _ => (["="]))
 in (LrTable.NT 10,(result,EQ1left,EQ1right),rest671) end
| (43,(_,(_,COMMA1left,COMMA1right))::rest671) => let val result=
MlyValue.exp(fn _ => ([","]))
 in (LrTable.NT 10,(result,COMMA1left,COMMA1right),rest671) end
| (44,(_,(_,COLON1left,COLON1right))::rest671) => let val result=
MlyValue.exp(fn _ => ([":"]))
 in (LrTable.NT 10,(result,COLON1left,COLON1right),rest671) end
| (45,(_,(_,SEMICOLON1left,SEMICOLON1right))::rest671) => let val 
result=MlyValue.exp(fn _ => ([";"]))
 in (LrTable.NT 10,(result,SEMICOLON1left,SEMICOLON1right),rest671)
 end
| (46,(_,(_,AND1left,AND1right))::rest671) => let val result=
MlyValue.exp(fn _ => (["and"]))
 in (LrTable.NT 10,(result,AND1left,AND1right),rest671) end
| (47,(_,(_,OF1left,OF1right))::rest671) => let val result=
MlyValue.exp(fn _ => (["of"]))
 in (LrTable.NT 10,(result,OF1left,OF1right),rest671) end
| (48,(_,(_,BAR1left,BAR1right))::rest671) => let val result=
MlyValue.exp(fn _ => (["|"]))
 in (LrTable.NT 10,(result,BAR1left,BAR1right),rest671) end
| (49,(_,(MlyValue.MLKEY MLKEY1,MLKEY1left,MLKEY1right))::rest671) => 
let val result=MlyValue.exp(fn _ => let val MLKEY as MLKEY1=MLKEY1 ()
 in ([MLKEY]) end
)
 in (LrTable.NT 10,(result,MLKEY1left,MLKEY1right),rest671) end
| (50,(_,(MlyValue.MLOP MLOP1,MLOP1left,MLOP1right))::rest671) => let 
val result=MlyValue.exp(fn _ => let val MLOP as MLOP1=MLOP1 ()
 in ([MLOP]) end
)
 in (LrTable.NT 10,(result,MLOP1left,MLOP1right),rest671) end
| (51,(_,(_,UMINUS1left,UMINUS1right))::rest671) => let val result=
MlyValue.exp(fn _ => (["~"]))
 in (LrTable.NT 10,(result,UMINUS1left,UMINUS1right),rest671) end
| (52,(_,(_,MAPSTO1left,MAPSTO1right))::rest671) => let val result=
MlyValue.exp(fn _ => (["=>"]))
 in (LrTable.NT 10,(result,MAPSTO1left,MAPSTO1right),rest671) end
| (53,(_,(MlyValue.DECINTRO DECINTRO1,DECINTRO1left,DECINTRO1right))::
rest671) => let val result=MlyValue.exp(fn _ => let val DECINTRO as 
DECINTRO1=DECINTRO1 ()
 in ([DECINTRO]) end
)
 in (LrTable.NT 10,(result,DECINTRO1left,DECINTRO1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : jacke_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun AS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun MAPSTO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun LOCAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ABSTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TOKEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSOCL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSOCR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun NONASSOC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RULE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun PARSER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun PREC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun SKIP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun DECINTRO (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.DECINTRO (fn () => i),p1,p2))
fun MLOP (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.MLOP (fn () => i),p1,p2))
fun MLKEY (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.MLKEY (fn () => i),p1,p2))
end
end
