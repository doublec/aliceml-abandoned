%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
export
   Interpreter
define
   NONE = 0
   SOME = 1

   IdDef    = 0
   Wildcard = 1

   Global = 0
   Local  = 1

   OneArg  = 0
   TupArgs = 1

   %Function = 0

   AppConst       = 0
   AppPrim        = 1
   AppVar         = 2
   ConTest        = 3
   EndHandle      = 4
   EndTry         = 5
   GetRef         = 6
   GetTup         = 7
   IntTest        = 8
   Kill           = 9
   PutCon         = 10
   PutConst       = 11
   PutFun         = 12
   PutNew         = 13
   PutRef         = 14
   PutTag         = 15
   PutTup         = 16
   PutVar         = 17
   PutVec         = 18
   Raise          = 19
   RealTest       = 20
   Reraise        = 21
   Return         = 22
   Shared         = 23
   StringTest     = 24
   TagTest        = 25
   Try            = 26
   VecTest        = 27
   %--** WideStringTest = 28

   Con       = 0
   StaticCon = 1

   fun {LitCase I N X YInstrVec ElseInstr}
      if I > N then ElseInstr
      elsecase YInstrVec.I of !X#ThenInstr then ThenInstr
      else {LitCase I + 1 N X YInstrVec ElseInstr}
      end
   end

   fun {TagCase I N T Cases}
      if I > N then unit
      elsecase Cases.I of Case=!T#_#_ then Case
      else {TagCase I + 1 N T Cases}
      end
   end

   fun {NullaryConCase I N C Cases G L ElseInstr}
      if I > N then ElseInstr
      elsecase Cases.I of Con0#ThenInstr then C2 in
	 C2 = case Con0 of tag(!Con IdRef) then
		 case IdRef of tag(!Local Id) then L.Id
		 [] tag(!Global J) then G.J
		 end
	      [] tag(!StaticCon X) then X
	      end
	 if C == C2 then ThenInstr
	 else {NullaryConCase I + 1 N C Cases G L ElseInstr}
	 end
      end
   end

   fun {NAryConCase I N C Cases G L}
      if I > N then unit
      elsecase Cases.I of Case=Con0#_#_ then C2 in
	 C2 = case Con0 of tag(!Con IdRef) then
		 case IdRef of tag(!Local Id) then L.Id
		 [] tag(!Global J) then G.J
		 end
	      [] tag(!StaticCon X) then X
	      end
	 if C == C2 then Case
	 else {NAryConCase I + 1 N C Cases G L}
	 end
      end
   end

   fun {VecCase I N J IdDefsInstrVec}
      if I > N then unit
      elsecase IdDefsInstrVec.I
      of Case=IdDefs#_ andthen {Width IdDefs} == J then Case
      else {VecCase I + 1 N J IdDefsInstrVec}
      end
   end

   fun {Emulate Instr G L TaskStack}
      case Instr of tag(!Kill Ids NextInstr) then
	 for I in 1..{Width Ids} do
	    L.(Ids.I) := unit
	 end
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutConst Id X NextInstr) then
	 L.Id := X
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutVar Id IdRef NextInstr) then
	 L.Id := case IdRef of tag(!Local Id2) then L.Id2
		 [] tag(!Global I) then G.I
		 end
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutNew Id NextInstr) then
	 L.Id := {NewName}
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutTag Id I IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple tag N + 1}
	 T.1 = I
	 for J in 1..N do
	    T.(J + 1) = case IdRefs.J of tag(!Local Id2) then L.Id2
			[] tag(!Global K) then G.K
			end
	 end
	 L.Id := T
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutCon Id Con0 IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple con N + 1}
	 T.1 = case Con0 of tag(!Con IdRef) then
		  case IdRef of tag(!Local Id2) then L.Id2
		  [] tag(!Global I) then G.I
		  end
	       [] tag(!StaticCon X) then X
	       end
	 for J in 1..N do
	    T.(J + 1) = case IdRefs.J of tag(!Local Id2) then L.Id2
			[] tag(!Global K) then G.K
			end
	 end
	 L.Id := T
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutRef Id IdRef NextInstr) then
	 L.Id := {NewCell case IdRef of tag(!Local Id2) then L.Id2
			  [] tag(!Global I) then G.I
			  end}
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutTup Id IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple tuple N}
	 for J in 1..N do
	    T.J = case IdRefs.J of tag(!Local Id2) then L.Id2
		  [] tag(!Global K) then G.K
		  end
	 end
	 L.Id := T
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutVec Id IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple vector N}
	 for J in 1..N do
	    T.J = case IdRefs.J of tag(!Local Id2) then L.Id2
		  [] tag(!Global K) then G.K
		  end
	 end
	 L.Id := T
	 {Emulate NextInstr G L TaskStack}
      [] tag(!PutFun Id IdRefs Function NextInstr) then N NewG in
	 N = {Width IdRefs}
	 NewG = {MakeTuple globals N}
	 for J in 1..N do
	    NewG.J = case IdRefs.J of tag(!Local Id2) then L.Id2
		     [] tag(!Global K) then G.K
		     end
	 end
	 L.Id := closure(Function NewG)
	 {Emulate NextInstr G L TaskStack}
      [] tag(!AppPrim Op IdRefs IdDefInstrOpt) then N Args in
	 N = {Width IdRefs}
	 Args = {MakeTuple args N}
	 for J in 1..N do X in
	    X = case IdRefs.J of tag(!Local Id) then L.Id
		[] tag(!Global K) then G.K
		end
	    Args.J = X
	 end
	 case IdDefInstrOpt of !NONE then   % tail call
	    continue(Args Op|TaskStack)
	 [] tag(!SOME tuple(IdDef NextInstr)) then NewFrame in
	    NewFrame = frame(Interpreter tag(!OneArg IdDef) NextInstr G L)
	    continue(Args Op|NewFrame|TaskStack)
	 end
      [] tag(!AppVar IdRef IdRefArgs IdDefArgsInstrOpt) then Op in
	 Op = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then G.I
	      end
	 {Emulate tag(AppConst Op IdRefArgs IdDefArgsInstrOpt) G L TaskStack}
      [] tag(!AppConst Op IdRefArgs IdDefArgsInstrOpt) then Args in
	 %% construct argument:
	 case IdRefArgs of tag(!OneArg IdRef) then
	    Args = arg(case IdRef of tag(!Local Id) then L.Id
		       [] tag(!Global I) then G.I
		       end)
	 [] tag(!TupArgs IdRefs) then N in
	    N = {Width IdRefs}
	    Args = {MakeTuple args N}
	    for J in 1..N do
	       Args.J = case IdRefs.J of tag(!Local Id) then L.Id
			[] tag(!Global K) then G.K
			end
	    end
	 end
	 case IdDefArgsInstrOpt of !NONE then   % tail call
	    continue(Args Op|TaskStack)
	 [] tag(!SOME tuple(IdDefArgs NextInstr)) then NewFrame in
	    NewFrame = frame(Interpreter IdDefArgs NextInstr G L)
	    continue(Args Op|NewFrame|TaskStack)
	 end
      [] tag(!GetRef Id IdRef NextInstr) then
	 L.Id := {Access case IdRef of tag(!Local Id2) then L.Id2
			 [] tag(!Global I) then G.I
			 end}
	 {Emulate NextInstr G L TaskStack}
      [] tag(!GetTup IdDefs IdRef NextInstr) then T N in
	 T = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then G.I
	     end
	 N = {Width IdDefs}
	 for J in 1..N do
	    case IdDefs.J of tag(!IdDef Id) then
	       L.Id := T.J
	    [] !Wildcard then skip
	    end
	 end
	 {Emulate NextInstr G L TaskStack}
      [] tag(!Raise IdRef) then Exn in
	 Exn = case IdRef of tag(!Local Id) then L.Id
	       [] tag(!Global I) then G.I
	       end
	 exception(nil Exn TaskStack)
      [] tag(!Reraise IdRef) then X in
	 X = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then G.I
	     end
	 case X of package(Debug Exn) then
	    exception(Debug Exn TaskStack)
	 end
      [] tag(!Try TryInstr IdDef1 IdDef2 HandleInstr) then
	 {Emulate TryInstr G L
	  handler(IdDef1 IdDef2 HandleInstr G L)|TaskStack}
      [] tag(!EndTry NextInstr) then
	 case TaskStack of handler(_ _ _ _)|Rest then
	    {Emulate NextInstr G L Rest}
	 end
      [] tag(!EndHandle NextInstr) then
	 {Emulate NextInstr G L TaskStack}
      [] tag(!IntTest IdRef IntInstrVec ElseInstr) then I ThenInstr in
	 I = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global J) then G.J
	     end
	 ThenInstr = {LitCase 1 {Width IntInstrVec} I IntInstrVec ElseInstr}
	 {Emulate ThenInstr G L TaskStack}
      [] tag(!RealTest IdRef RealInstrVec ElseInstr) then F ThenInstr in
	 F = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then G.I
	     end
	 ThenInstr = {LitCase 1 {Width RealInstrVec} F RealInstrVec ElseInstr}
	 {Emulate ThenInstr G L TaskStack}
      [] tag(!StringTest IdRef StringInstrVec ElseInstr) then S ThenInstr in
	 S = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then G.I
	     end
	 ThenInstr = {LitCase 1 {Width StringInstrVec}
		      S StringInstrVec ElseInstr}
	 {Emulate ThenInstr G L TaskStack}
/*--**
      [] tag(!WideStringTest IdRef _ ElseInstr) then
	 %--**
	 {Emulate ElseInstr G L TaskStack}
*/
      [] tag(!TagTest IdRef NullaryCases NAryCases ElseInstr) then T in
	 T = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then G.I
	     end
	 if {IsInt T} then ThenInstr in
	    ThenInstr = {LitCase 1 {Width NullaryCases}
			 T NullaryCases ElseInstr}
	    {Emulate ThenInstr G L TaskStack}
	 elsecase {TagCase 1 {Width NAryCases} T.1 NAryCases}
	 of _#IdDefs#ThenInstr then N in
	    N = {Width IdDefs}
	    for J in 1..N do
	       case IdDefs.J of tag(!IdDef Id) then
		  L.Id := T.(J + 1)
	       [] !Wildcard then skip
	       end
	    end
	    {Emulate ThenInstr G L TaskStack}
	 [] unit then
	    {Emulate ElseInstr G L TaskStack}
	 end
      [] tag(!ConTest IdRef NullaryCases NAryCases ElseInstr) then C in
	 C = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then G.I
	     end
	 if {IsName C} then ThenInstr in
	    ThenInstr = {NullaryConCase 1 {Width NullaryCases}
			 C NullaryCases G L ElseInstr}
	    {Emulate ThenInstr G L TaskStack}
	 elsecase {NAryConCase 1 {Width NAryCases} C.1 NAryCases G L}
	 of _#IdDefs#ThenInstr then N in
	    N = {Width IdDefs}
	    for J in 1..N do
	       case IdDefs.J of tag(!IdDef Id) then
		  L.Id := C.(J + 1)
	       [] !Wildcard then skip
	       end
	    end
	    {Emulate ThenInstr G L TaskStack}
	 [] unit then
	    {Emulate ElseInstr G L TaskStack}
	 end
      [] tag(!VecTest IdRef IdDefsInstrVec ElseInstr) then V in
	 V = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then G.I
	     end
	 case {VecCase 1 {Width IdDefsInstrVec} {Width V} IdDefsInstrVec}
	 of IdDefs#ThenInstr then N in
	    N = {Width IdDefs}
	    for J in 1..N do
	       case IdDefs.J of tag(!IdDef Id) then
		  L.Id := V.J
	       [] !Wildcard then skip
	       end
	    end
	    {Emulate ThenInstr G L TaskStack}
	 [] unit then
	    {Emulate ElseInstr G L TaskStack}
	 end
      [] tag(!Shared _ NextInstr) then
	 {Emulate NextInstr G L TaskStack}
      [] tag(!Return IdRefArgs) then Args in
	 %% construct arguments to call the continuation with:
	 case IdRefArgs of tag(!OneArg IdRef) then
	    Args = arg(case IdRef of tag(!Local Id) then L.Id
		       [] tag(!Global I) then G.I
		       end)
	 [] tag(!TupArgs IdRefs) then N in
	    N = {Width IdRefs}
	    Args = {MakeTuple args N}
	    for J in 1..N do
	       Args.J = case IdRefs.J of tag(!Local Id) then L.Id
			[] tag(!Global K) then G.K
			end
	    end
	 end
	 continue(Args TaskStack)
      end
   end

   fun {Run Args TaskStack}
      case TaskStack of frame(_ IdDefArgs Instr G L)|Rest then
	 case IdDefArgs of tag(!OneArg IdDef) then
	    case IdDef of tag(!IdDef Id) then
	       L.Id := case Args of arg(X) then X
		       [] args(...) then {Adjoin Args tuple}
		       end
	    [] !Wildcard then skip
	    end
	 [] tag(!TupArgs IdDefs) then T N in
	    T = case Args of arg(X) then X
		[] args(...) then Args
		end
	    N = {Width IdDefs}
	    for J in 1..N do
	       case IdDefs.J of tag(!IdDef Id) then
		  L.Id := T.J
	       [] !Wildcard then skip
	       end
	    end
	 end
	 {Emulate Instr G L Rest}
      [] nil then terminate
      end
   end

   fun {Handle Debug Exn TaskStack}
      case TaskStack of handler(IdDef1 IdDef2 Instr G L)|Rest then
	 case IdDef1 of tag(!IdDef Id) then
	    L.Id := package(Debug Exn)
	 [] !Wildcard then skip
	 end
	 case IdDef2 of tag(!IdDef Id) then
	    L.Id := Exn
	 [] !Wildcard then skip
	 end
	 {Emulate Instr G L Rest}
      [] Frame=frame(_ _ _ _ _)|Rest then
	 exception(Frame|Debug Exn Rest)
      end
   end

   Interpreter = abstractCodeInterpreter(run: Run
					 handle: Handle)
end
