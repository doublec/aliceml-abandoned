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
import
   BootName(newNamed: NewNamedName) at 'x-oz://boot/Name'
export
   MakeConcreteCode
require
   Helper(deref: Deref pushCall: PushCall)
define
   NONE = 0
   SOME = 1

   IdDef    = 0
   Wildcard = 1

   OneArg  = 0
   TupArgs = 1

   AppPrim        = 0
   AppVar         = 1
   Close          = 2
   CompactIntTest = 3
   CompactTagTest = 4
   ConTest        = 5
   DirectAppVar   = 6
   EndHandle      = 7
   EndTry         = 8
   GetRef         = 9
   GetTup         = 10
   IntTest        = 11
   Kill           = 12
   LazyPolySel    = 13
   PutCon         = 14
   PutNew         = 15
   PutRef         = 16
   PutTag         = 17
   PutTup         = 18
   PutPolyRec     = 19
   PutVar         = 20
   PutVec         = 21
   Raise          = 22
   RealTest       = 23
   Reraise        = 24
   Return         = 25
   Sel            = 26
   Shared         = 27
   Specialize     = 28
   StringTest     = 29
   TagTest        = 30
   Try            = 31
   VecTest        = 32

   Global       = 0
   Immediate    = 1
   LastUseLocal = 2
   Local        = 3
   Toplevel     = 4

   Template = 0

   Function    = 0
   Specialized = 1

   AliceFunction = {ByteString.make 'Alice.function'}

   fun {MakeConcreteCode AbstractCode}
      case AbstractCode
      of tag(!Function tuple(F L C) NG NL IdDefArgs Instr) then
	 function(Me {VirtualString.toAtom F}#L#C NG NL IdDefArgs Instr
		  transform(AliceFunction AbstractCode))
      [] tag(!Specialized tuple(F L C) Xs NL IdDefArgs Instr) then
	 specialized(Me {VirtualString.toAtom F}#L#C Xs NL IdDefArgs Instr
		     transform(AliceFunction AbstractCode))
      end
   end

   fun {PolySel T A I}
      if T.(2 * I) == A then T.(2 * I + 1)
      elseif I \= T.1 then {PolySel T A I + 1}
      end
   end

   LazySelInterpreter =
   lazySelInterpreter(
      run:
	 fun {$ Args TaskStack}
	    case TaskStack of lazySelFrame(_ X uniqueString(A))|Rest then
	       case {Deref X} of Transient=transient(_) then
		  request(Transient Args TaskStack)
	       elseof T then
		  continue(arg({PolySel T A 1}) Rest)
	       end
	    end
	 end
      handle:
	 fun {$ Debug Exn TaskStack}
	    case TaskStack of (Frame=lazySelFrame(_ _ _))|Rest then
	       exception(Frame|Debug Exn Rest)
	    end
	 end
      pushCall:
	 fun {$ Closure _ TaskStack}
	    case Closure of closure(_ X Label) then
	       lazySelFrame(LazySelInterpreter X Label)|TaskStack
	    end
	 end
      toString:
	 fun {$ lazySelFrame(_ _ uniqueString(A))} 'Select '#A end)

   fun {GetIdRef IdRef Closure L}
      case IdRef of tag(!Immediate X) then X
      [] tag(!Local Id) then L.Id
      [] tag(!LastUseLocal Id) then L.Id
      [] tag(!Global I) then Closure.(I + 2)
      [] tag(!Toplevel I) then Closure.(I + 2)
      end
   end

   proc {KillIdRef IdRef L}
      case IdRef of tag(!LastUseLocal Id) then L.Id := dead1
      else skip
      end
   end

   fun {GetIdRefKill IdRef Closure L}
      case IdRef of tag(!Immediate X) then X
      [] tag(!Local Id) then L.Id
      [] tag(!LastUseLocal Id) then
	 X = L.Id in L.Id := dead2 X
      [] tag(!Global I) then Closure.(I + 2)
      [] tag(!Toplevel I) then Closure.(I + 2)
      end
   end

   fun {LitCase I N X YInstrVec ElseInstr}
      if I > N then ElseInstr
      elsecase YInstrVec.I of tuple(!X ThenInstr) then ThenInstr
      else {LitCase I + 1 N X YInstrVec ElseInstr}
      end
   end

   fun {TagCase I N T Cases}
      if I > N then unit
      elsecase Cases.I of Case=tuple(!T _ _) then Case
      else {TagCase I + 1 N T Cases}
      end
   end

   fun {NullaryConCase I N C Cases Closure L ElseInstr}
      %--** some kills missing
      if I > N then ElseInstr
      elsecase Cases.I of tuple(IdRef ThenInstr) then C2 in
	 C2 = {GetIdRef IdRef Closure L}
	 case {Deref C2} of Transient=transient(_) then request(Transient)
	 elseof C3 then
	    if C == C3 then ThenInstr
	    else {NullaryConCase I + 1 N C Cases Closure L ElseInstr}
	    end
	 end
      end
   end

   fun {NAryConCase I N C Cases Closure L}
      %--** some kills missing
      if I > N then unit
      elsecase Cases.I of Case=tuple(IdRef _ _) then C2 in
	 C2 = {GetIdRef IdRef Closure L}
	 case {Deref C2} of Transient=transient(_) then request(Transient)
	 elseof C3 then
	    if C == C3 then Case
	    else {NAryConCase I + 1 N C Cases Closure L}
	    end
	 end
      end
   end

   fun {VecCase I N J IdDefsInstrVec}
      if I > N then unit
      elsecase IdDefsInstrVec.I
      of Case=tuple(IdDefs _) andthen {Width IdDefs} == J then Case
      else {VecCase I + 1 N J IdDefsInstrVec}
      end
   end

   fun {Emulate Instr Closure L TaskStack}
      %--** preemption
      case Instr of tag(!Kill Ids NextInstr) then
	 for I in 1..{Width Ids} do
	    L.(Ids.I) := dead3
	 end
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutVar Id IdRef NextInstr) then
	 L.Id := {GetIdRefKill IdRef Closure L}
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutNew Id S NextInstr) then
	 L.Id := case {VirtualString.toAtom S} of '' then {NewName}
		 elseof A then {NewNamedName A}
		 end
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutTag Id I IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple tag N + 1}
	 T.1 = I
	 for J in 1..N do
	    T.(J + 1) = {GetIdRefKill IdRefs.J Closure L}
	 end
	 L.Id := T
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutCon Id IdRef IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple con N + 1}
	 T.1 = {GetIdRefKill IdRef Closure L}
	 for J in 1..N do
	    T.(J + 1) = {GetIdRefKill IdRefs.J Closure L}
	 end
	 L.Id := T
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutRef Id IdRef NextInstr) then
	 L.Id := {NewCell {GetIdRefKill IdRef Closure L}}
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutTup Id IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple tuple N}
	 for J in 1..N do
	    T.J = {GetIdRefKill IdRefs.J Closure L}
	 end
	 L.Id := T
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutPolyRec Id Labels IdRefs NextInstr) then
	 N = {Width Labels}
	 R = {MakeTuple record N * 2 + 1}
	 R.1 = N
	 for J in 1..N do
	    R.(J * 2) = Labels.J
	    R.(J * 2 + 1) = {GetIdRefKill IdRefs.J Closure L}
	 end
	 L.Id := R
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutVec Id IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple vector N}
	 for J in 1..N do
	    T.J = {GetIdRefKill IdRefs.J Closure L}
	 end
	 L.Id := T
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!Close Id IdRefs ConcreteCode NextInstr) then N NewClosure in
	 N = {Width IdRefs}
	 NewClosure = {MakeTuple closure N + 1}
	 NewClosure.1 = ConcreteCode
	 for J in 1..N do
	    NewClosure.(J + 1) = {GetIdRefKill IdRefs.J Closure L}
	 end
	 L.Id := NewClosure
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!Specialize Id IdRefs
	     tag(!Template Coord NT NL IdDefArgs Instr) NextInstr)
      then Toplevels AbstractCode NewClosure in
	 Toplevels = {MakeTuple vector NT}
	 AbstractCode = tag(Specialized Coord Toplevels NL IdDefArgs Instr)
	 NewClosure = {MakeTuple closure NT + 1}
	 NewClosure.1 = {MakeConcreteCode AbstractCode}
	 for J in 1..NT do X in
	    X = {GetIdRefKill IdRefs.J Closure L}
	    Toplevels.J = X
	    NewClosure.(J + 1) = X
	 end
	 L.Id := NewClosure
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!AppPrim Op IdRefs IdDefInstrOpt) then Args in
	 case {Width IdRefs} of 1 then
	    Args = arg({GetIdRefKill IdRefs.1 Closure L})
	 elseof N then
	    Args = {MakeTuple args N}
	    for J in 1..N do
	       Args.J = {GetIdRefKill IdRefs.J Closure L}
	    end
	 end
	 case IdDefInstrOpt of !NONE then   % tail call
	    {PushCall Args Op TaskStack}
	 [] tag(!SOME tuple(IdDef NextInstr)) then NewFrame in
	    NewFrame = frame(Me tag(!OneArg IdDef) NextInstr Closure L)
	    {PushCall Args Op NewFrame|TaskStack}
	 end
      [] tag(!AppVar IdRef IdRefArgs IdDefArgsInstrOpt) then Op Args in
	 Op = {GetIdRefKill IdRef Closure L}
	 %% construct argument:
	 case IdRefArgs of tag(!OneArg IdRef) then
	    Args = arg({GetIdRefKill IdRef Closure L})
	 [] tag(!TupArgs IdRefs) then N in
	    N = {Width IdRefs}
	    Args = {MakeTuple args N}
	    for J in 1..N do
	       Args.J = {GetIdRefKill IdRefs.J Closure L}
	    end
	 end
	 case IdDefArgsInstrOpt of !NONE then   % tail call
	    {PushCall Args Op TaskStack}
	 [] tag(!SOME tuple(IdDefArgs NextInstr)) then NewFrame in
	    NewFrame = frame(Me IdDefArgs NextInstr Closure L)
	    {PushCall Args Op NewFrame|TaskStack}
	 end
      [] tag(!DirectAppVar IdRef IdRefArgs IdDefArgsInstrOpt) then
	 {Emulate tag(AppVar IdRef IdRefArgs IdDefArgsInstrOpt)
	  Closure L TaskStack}
      [] tag(!GetRef Id IdRef NextInstr) then R0 in
	 R0 = {GetIdRef IdRef Closure L}
	 case {Deref R0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof R then
	    {KillIdRef IdRef L}
	    L.Id := {Access R}
	    {Emulate NextInstr Closure L TaskStack}
	 end
      [] tag(!GetTup IdDefs IdRef NextInstr) then T0 in
	 T0 = {GetIdRef IdRef Closure L}
	 case {Deref T0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof T then N in
	    {KillIdRef IdRef L}
	    N = {Width IdDefs}
	    for J in 1..N do
	       case IdDefs.J of tag(!IdDef Id) then
		  L.Id := T.J
	       [] !Wildcard then skip
	       end
	    end
	    {Emulate NextInstr Closure L TaskStack}
	 end
      [] tag(!Sel Id IdRef I NextInstr) then T0 in
	 T0 = {GetIdRef IdRef Closure L}
	 case {Deref T0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof T then
	    {KillIdRef IdRef L}
	    L.Id := T.(I + 1)
	    {Emulate NextInstr Closure L TaskStack}
	 end
      [] tag(!LazyPolySel Id IdRef Label NextInstr) then X NewClosure in
	 X = {GetIdRefKill IdRef Closure L}
	 NewClosure = closure(lazySel(LazySelInterpreter) X Label)
	 L.Id := transient({NewCell byneed(NewClosure)})
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!Raise IdRef) then Exn in
	 Exn = {GetIdRefKill IdRef Closure L}
	 exception(nil Exn TaskStack)
      [] tag(!Reraise IdRef) then
	 case {GetIdRefKill IdRef Closure L} of package(Debug Exn) then
	    exception(Debug Exn TaskStack)
	 end
      [] tag(!Try TryInstr IdDef1 IdDef2 HandleInstr) then
	 {Emulate TryInstr Closure L
	  handler(Me IdDef1 IdDef2 HandleInstr Closure L)|TaskStack}
      [] tag(!EndTry NextInstr) then
	 case TaskStack of handler(_ _ _ _ _ _)|Rest then
	    {Emulate NextInstr Closure L Rest}
	 end
      [] tag(!EndHandle NextInstr) then
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!IntTest IdRef IntInstrVec ElseInstr) then I0 in
	 I0 = {GetIdRef IdRef Closure L}
	 case {Deref I0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof I then ThenInstr in
	    {KillIdRef IdRef L}
	    ThenInstr = {LitCase 1 {Width IntInstrVec} I IntInstrVec ElseInstr}
	    {Emulate ThenInstr Closure L TaskStack}
	 end
      [] tag(!CompactIntTest IdRef Offset Instrs ElseInstr) then I0 in
	 I0 = {GetIdRef IdRef Closure L}
	 case {Deref I0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof I then Index in
	    {KillIdRef IdRef L}
	    Index = I - Offset
	    if Index >= 0 andthen Index < {Width Instrs} then
	       {Emulate Instrs.(Index + 1) Closure L TaskStack}
	    else
	       {Emulate ElseInstr Closure L TaskStack}
	    end
	 end
      [] tag(!RealTest IdRef RealInstrVec ElseInstr) then F0 in
	 F0 = {GetIdRef IdRef Closure L}
	 case {Deref F0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof F then ThenInstr in
	    {KillIdRef IdRef L}
	    ThenInstr = {LitCase 1 {Width RealInstrVec}
			 F RealInstrVec ElseInstr}
	    {Emulate ThenInstr Closure L TaskStack}
	 end
      [] tag(!StringTest IdRef StringInstrVec ElseInstr) then S0 in
	 S0 = {GetIdRef IdRef Closure L}
	 case {Deref S0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof S then ThenInstr in
	    {KillIdRef IdRef L}
	    ThenInstr = {LitCase 1 {Width StringInstrVec}
			 S StringInstrVec ElseInstr}
	    {Emulate ThenInstr Closure L TaskStack}
	 end
      [] tag(!TagTest IdRef NullaryCases NAryCases ElseInstr) then T0 in
	 T0 = {GetIdRef IdRef Closure L}
	 case {Deref T0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof T then
	    {KillIdRef IdRef L}
	    if {IsInt T} then ThenInstr in
	       ThenInstr = {LitCase 1 {Width NullaryCases}
			    T NullaryCases ElseInstr}
	       {Emulate ThenInstr Closure L TaskStack}
	    elsecase {TagCase 1 {Width NAryCases} T.1 NAryCases}
	    of tuple(_ IdDefs ThenInstr) then
	       for J in 1..{Width IdDefs} do
		  case IdDefs.J of tag(!IdDef Id) then
		     L.Id := T.(J + 1)
		  [] !Wildcard then skip
		  end
	       end
	       {Emulate ThenInstr Closure L TaskStack}
	    [] unit then
	       {Emulate ElseInstr Closure L TaskStack}
	    end
	 end
      [] tag(!CompactTagTest IdRef Tests ElseInstr) then T0 in
	 T0 = {GetIdRef IdRef Closure L}
	 case {Deref T0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof T then Index in
	    {KillIdRef IdRef L}
	    Index = if {IsInt T} then T else T.1 end
	    if Index >= {Width Tests} then
	       {Emulate ElseInstr Closure L TaskStack}
	    elsecase Tests.(Index + 1) of tuple(!NONE ThenInstr) then
	       {Emulate ThenInstr Closure L TaskStack}
	    [] tuple(tag(!SOME IdDefs) ThenInstr) then
	       for J in 1..{Width IdDefs} do
		  case IdDefs.J of tag(!IdDef Id) then
		     L.Id := T.(J + 1)
		  [] !Wildcard then skip
		  end
	       end
	       {Emulate ThenInstr Closure L TaskStack}
	    end
	 end
      [] tag(!ConTest IdRef NullaryCases NAryCases ElseInstr) then C0 in
	 C0 = {GetIdRef IdRef Closure L}
	 case {Deref C0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof C then
	    if {IsName C} then
	       case {NullaryConCase 1 {Width NullaryCases}
		     C NullaryCases Closure L ElseInstr}
	       of request(Transient) then NewFrame in
		  NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
		  request(Transient args() NewFrame|TaskStack)
	       elseof ThenInstr then
		  {KillIdRef IdRef L}
		  {Emulate ThenInstr Closure L TaskStack}
	       end
	    elsecase {Deref C.1} of Transient=transient(_) then NewFrame in
	       NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	       request(Transient args() NewFrame|TaskStack)
	    elseof C1 then
	       case {NAryConCase 1 {Width NAryCases} C1 NAryCases Closure L}
	       of tuple(_ IdDefs ThenInstr) then N in
		  N = {Width IdDefs}
		  for J in 1..N do
		     case IdDefs.J of tag(!IdDef Id) then
			L.Id := C.(J + 1)
		     [] !Wildcard then skip
		     end
		  end
		  {KillIdRef IdRef L}
		  {Emulate ThenInstr Closure L TaskStack}
	       [] request(Transient) then NewFrame in
		  NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
		  request(Transient args() NewFrame|TaskStack)
	       [] unit then
		  {KillIdRef IdRef L}
		  {Emulate ElseInstr Closure L TaskStack}
	       end
	    end
	 end
      [] tag(!VecTest IdRef IdDefsInstrVec ElseInstr) then V0 in
	 V0 = {GetIdRef IdRef Closure L}
	 case {Deref V0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof V then
	    {KillIdRef IdRef L}
	    case {VecCase 1 {Width IdDefsInstrVec} {Width V} IdDefsInstrVec}
	    of tuple(IdDefs ThenInstr) then N in
	       N = {Width IdDefs}
	       for J in 1..N do
		  case IdDefs.J of tag(!IdDef Id) then
		     L.Id := V.J
		  [] !Wildcard then skip
		  end
	       end
	       {Emulate ThenInstr Closure L TaskStack}
	    [] unit then
	       {Emulate ElseInstr Closure L TaskStack}
	    end
	 end
      [] tag(!Shared _ NextInstr) then
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!Return IdRefArgs) then Args in
	 %% construct arguments to call the continuation with:
	 case IdRefArgs of tag(!OneArg IdRef) then
	    Args = arg({GetIdRefKill IdRef Closure L})
	 [] tag(!TupArgs IdRefs) then N in
	    N = {Width IdRefs}
	    Args = {MakeTuple args N}
	    for J in 1..N do
	       Args.J = {GetIdRefKill IdRefs.J Closure L}
	    end
	 end
	 continue(Args TaskStack)
      end
   end

   fun {Run Args TaskStack}
      try
	 case TaskStack of (Frame=frame(_ IdDefArgs Instr Closure L))|Rest then
	    case IdDefArgs of tag(!OneArg IdDef0) then
	       case IdDef0 of tag(!IdDef Id) then
		  L.Id := case Args of arg(X) then X
			  [] args(...) then {Adjoin Args tuple}   % "construct"
			  end
	       [] !Wildcard then skip
	       end
	    [] tag(!TupArgs IdDefs) then T N in
	       T = case Args of arg(X0) then
		      case {Deref X0} of Transient=transient(_) then
			 raise request(Transient Args TaskStack) end
		      elseof X then X   % "deconstruct"
		      end
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
	    case {Emulate Instr Closure L Rest}
	    of exception(nil Exn TaskStack) then
	       exception([Frame] Exn TaskStack)
	    elseof Res then Res
	    end
	 end
      catch Request=request(_ _ _) then Request
      end
   end

   fun {Handle Debug Exn TaskStack}
      case TaskStack of handler(_ IdDef1 IdDef2 Instr Closure L)|Rest then
	 case IdDef1 of tag(!IdDef Id) then
	    L.Id := package(Debug Exn)
	 [] !Wildcard then skip
	 end
	 case IdDef2 of tag(!IdDef Id) then
	    L.Id := Exn
	 [] !Wildcard then skip
	 end
	 {Emulate Instr Closure L Rest}
      [] (Frame=frame(_ _ _ _ _))|Rest then
	 exception(Frame|Debug Exn Rest)
      end
   end

   Me =
   abstractCodeInterpreter(
      run: Run
      handle: Handle
      pushCall:
	 fun {$ Closure Function TaskStack}
	    case Function of function(_ _ _ NL IdDefArgs BodyInstr _) then L in
	       L = {NewArray 0 NL - 1 uninitialized}
	       frame(Me IdDefArgs BodyInstr Closure L)|TaskStack
	    [] specialized(_ _ _ NL IdDefArgs BodyInstr _) then L in
	       L = {NewArray 0 NL - 1 uninitialized}
	       frame(Me IdDefArgs BodyInstr Closure L)|TaskStack
	    end
	 end
      abstract:
	 fun {$ ConcreteCode}
	    case ConcreteCode of function(_ _ _ _ _ _ Transform) then Transform
	    [] specialized(_ _ _ _ _ _ Transform) then Transform
	    end
	 end
      toString:
	 fun {$ Frame}
	    case Frame of frame(_ _ _ closure(Function ...) _) then
	       case {Deref Function} of function(_ F#L#C _ _ _ _ _) then
		  'Alice function '#F#', line '#L#', column '#C
	       [] specialized(_ F#L#C _ _ _ _ _) then
		  'Alice specialized function '#F#', line '#L#', column '#C
	       end
	    [] handler(_ _ _ _ closure(Function ...) _) then
	       case {Deref Function} of function(_ F#L#C _ _ _ _ _) then
		  'Alice handler '#F#', line '#L#', column '#C
	       [] specialized(_ F#L#C _ _ _ _ _) then
		  'Alice specialized handler '#F#', line '#L#', column '#C
	       end
	    end
	 end)
end
