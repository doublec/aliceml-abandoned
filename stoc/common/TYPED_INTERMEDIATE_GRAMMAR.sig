signature INTERMEDIATE_TYPE =
  sig

    type lab  = Lab.t
    type path = Path.t

    datatype typ =
	  ARR  of typ * typ		(* arrow type *)
	| TUP  of typ list		(* tuple *)
	| ROW  of row			(* record *)
	| SUM  of row			(* sum type (datatype) *)
	| CON  of path * typ list	(* constructed type *)
	| VAR				(* generic variable or skolem type *)

    withtype typ = typ' ref
    and      row = (lab * typ) list * bool

  end

signature INTERMEDIATE_GRAMMAR =
  sig

    (* Generic *)

    type info	(* = Source.position * Type.typ *)

    (* Literals *)

    datatype lit =
	  WordLit   of LargeWord.word		(* modulo arithmetic *)
	| IntLit    of LargeInt.int		(* integer arithmetic *)
	| CharLit   of WideChar.char		(* character *)
	| StringLit of WideString.string	(* character string *)
	| RealLit   of LargeReal.real		(* floating point *)

    (* Identifiers *)

    datatype lab    = Lab     of info * Lab.t
    datatype id     = Id      of info * Stamp.t * Name.t
    datatype longid = ShortId of info * id
		    | LongId  of info * longid * lab

    (* Expressions *)

    datatype exp =
	  LitExp    of info * lit		(* literal *)
	| PrimExp   of info * string		(* primitive value *)
	| NewExp    of info			(* new constructor *)
	| VarExp    of info * longid		(* variable *)
	| ConExp    of info * longid		(* constructor *)
	| RefExp    of info			(* reference constructor *)
	| TupExp    of info * exp list		(* tuple *)
	| RowExp    of info * exp field list	(* record / module *)
			(* all labels distinct *)
	| SelExp    of info * lab		(* field selection *)
	| VecExp    of info * exp list		(* vector *)
	| FunExp    of info * id * exp		(* function *)
	| AppExp    of info * exp * exp		(* application *)
	| AdjExp    of info * exp * exp		(* record adjoin *)
	| UpExp     of info * exp		(* up cast *)
	| AndExp    of info * exp * exp		(* conjunction *)
	| OrExp     of info * exp * exp		(* disjunction *)
	| IfExp     of info * exp * exp * exp	(* conditional *)
	| WhileExp  of info * exp * exp		(* conditional loop *)
	| SeqExp    of info * exp list		(* sequential *)
	| CaseExp   of info * exp * match list	(* case switch *)
	| RaiseExp  of info * exp		(* exception raise *)
	| HandleExp of info * exp * match list	(* exception handling *)
	| LetExp    of info * dec list * exp	(* local binding *)

    and 'a field = Field of info * lab * 'a

    and match    = Match of info * pat * exp

    (* Patterns (always linear) *)

    and pat =
	  WildPat   of info			(* wildcard *)
	| LitPat    of info * lit		(* literal *)
	| VarPat    of info * id		(* variable *)
	| ConPat    of info * longid * pat option (* constructor pattern *)
			(* pat present iff longid has arguments *)
	| RefPat    of info * pat		(* reference pattern *)
	| TupPat    of info * pat list		(* tuple *)
	| RowPat    of info * pat field list	(* record *)
			(* all labels distinct *)
	| VecPat    of info * pat list		(* vector *)
	| AsPat     of info * pat * pat		(* pattern conjunction *)
	| AltPat    of info * pat list		(* pattern disjunction *)
			(* all patterns bind same ids *)
	| NegPat    of info * pat		(* pattern negation *)
	| GuardPat  of info * pat * exp		(* guarded pattern *)
	| WithPat   of info * pat * dec list	(* local bindings *)

    (* Declarations *)

    and dec =
	  ValDec    of info * pat * exp		(* value / module *)
	| RecDec    of info * dec list		(* recursive definition *)

    (* Components *)

    type comp = (lab * string) list * exp	(* exp is a function! *)


    (* Operations *)

    val infoLab :	lab	-> info
    val infoId :	id	-> info
    val infoLongid :	longid	-> info
    val infoExp :	exp	-> info
    val infoField :	'a field-> info
    val infoMatch :	match	-> info
    val infoPat :	pat	-> info
    val infoDec :	dec	-> info

  end
