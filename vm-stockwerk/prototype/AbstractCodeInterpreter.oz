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
   interpreter: Me
require
   Helper(deref: Deref pushCall: PushCall)
define
   NONE = 0
   SOME = 1

   IdDef    = 0
   Wildcard = 1

   OneArg  = 0
   TupArgs = 1

   %Function = 0

   AppPrim        = 0
   AppVar         = 1
   CompactIntTest = 2
   CompactTagTest = 3
   ConTest        = 4
   DirectAppVar   = 5
   EndHandle      = 6
   EndTry         = 7
   GetRef         = 8
   GetTup         = 9
   IntTest        = 10
   Kill           = 11
   LazySel        = 12
   PutCon         = 13
   PutFun         = 14
   PutNew         = 15
   PutRef         = 16
   PutTag         = 17
   PutTup         = 18
   PutVar         = 19
   PutVec         = 20
   Raise          = 21
   RealTest       = 22
   Reraise        = 23
   Return         = 24
   Sel            = 25
   Shared         = 26
   StringTest     = 27
   TagTest        = 28
   Try            = 29
   VecTest        = 30

   Global    = 0
   Immediate = 1
   Local     = 2

   LazySelInterpreter =
   lazySelInterpreter(
      run:
	 fun {$ Args TaskStack}
	    case TaskStack of lazySelFrame(_ X I)|Rest then
	       case {Deref X} of Transient=transient(_) then
		  request(Transient Args TaskStack)
	       elseof T then
		  continue(arg(T.(I + 1)) Rest)
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
	    case Closure of closure(_ X I) then
	       lazySelFrame(LazySelInterpreter X I)|TaskStack
	    end
	 end
      toString:
	 fun {$ lazySelFrame(_ _ I)}
	    'Select #'#I
	 end)

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
      if I > N then ElseInstr
      elsecase Cases.I of tuple(IdRef ThenInstr) then C2 in
	 C2 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global J) then Closure.(J + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref C2} of Transient=transient(_) then request(Transient)
	 elseof C3 then
	    if C == C3 then ThenInstr
	    else {NullaryConCase I + 1 N C Cases Closure L ElseInstr}
	    end
	 end
      end
   end

   fun {NAryConCase I N C Cases Closure L}
      if I > N then unit
      elsecase Cases.I of Case=tuple(IdRef _ _) then C2 in
	 C2 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global J) then Closure.(J + 2)
	      [] tag(!Immediate X) then X
	      end
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
	    L.(Ids.I) := killed
	 end
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutVar Id IdRef NextInstr) then
	 L.Id := case IdRef of tag(!Local Id2) then L.Id2
		 [] tag(!Global I) then Closure.(I + 2)
		 [] tag(!Immediate X) then X
		 end
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
	    T.(J + 1) = case IdRefs.J of tag(!Local Id2) then L.Id2
			[] tag(!Global K) then Closure.(K + 2)
			[] tag(!Immediate X) then X
			end
	 end
	 L.Id := T
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutCon Id IdRef IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple con N + 1}
	 T.1 = case IdRef of tag(!Local Id2) then L.Id2
	       [] tag(!Global I) then Closure.(I + 2)
	       [] tag(!Immediate X) then X
	       end
	 for J in 1..N do
	    T.(J + 1) = case IdRefs.J of tag(!Local Id2) then L.Id2
			[] tag(!Global K) then Closure.(K + 2)
			[] tag(!Immediate X) then X
			end
	 end
	 L.Id := T
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutRef Id IdRef NextInstr) then
	 L.Id := {NewCell case IdRef of tag(!Local Id2) then L.Id2
			  [] tag(!Global I) then Closure.(I + 2)
			  [] tag(!Immediate X) then X
			  end}
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutTup Id IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple tuple N}
	 for J in 1..N do
	    T.J = case IdRefs.J of tag(!Local Id2) then L.Id2
		  [] tag(!Global K) then Closure.(K + 2)
		  [] tag(!Immediate X) then X
		  end
	 end
	 L.Id := T
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutVec Id IdRefs NextInstr) then N T in
	 N = {Width IdRefs}
	 T = {MakeTuple vector N}
	 for J in 1..N do
	    T.J = case IdRefs.J of tag(!Local Id2) then L.Id2
		  [] tag(!Global K) then Closure.(K + 2)
		  [] tag(!Immediate X) then X
		  end
	 end
	 L.Id := T
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!PutFun Id IdRefs Function NextInstr) then N NewClosure in
	 N = {Width IdRefs}
	 NewClosure = {MakeTuple closure N + 1}
	 NewClosure.1 = Function
	 for J in 1..N do
	    NewClosure.(J + 1) = case IdRefs.J of tag(!Local Id2) then L.Id2
				 [] tag(!Global K) then Closure.(K + 2)
				 [] tag(!Immediate X) then X
				 end
	 end
	 L.Id := NewClosure
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!AppPrim Op IdRefs IdDefInstrOpt) then Args in
	 case {Width IdRefs} of 1 then
	    Args = arg(case IdRefs.1 of tag(!Local Id) then L.Id
		       [] tag(!Global K) then Closure.(K + 2)
		       [] tag(!Immediate X) then X
		       end)
	 elseof N then
	    Args = {MakeTuple args N}
	    for J in 1..N do
	       Args.J = case IdRefs.J of tag(!Local Id) then L.Id
			[] tag(!Global K) then Closure.(K + 2)
			[] tag(!Immediate X) then X
			end
	    end
	 end
	 case IdDefInstrOpt of !NONE then   % tail call
	    {PushCall Args Op TaskStack}
	 [] tag(!SOME tuple(IdDef NextInstr)) then NewFrame in
	    NewFrame = frame(Me tag(!OneArg IdDef) NextInstr Closure L)
	    {PushCall Args Op NewFrame|TaskStack}
	 end
      [] tag(!AppVar IdRef IdRefArgs IdDefArgsInstrOpt) then Op Args in
	 Op = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 %% construct argument:
	 case IdRefArgs of tag(!OneArg IdRef) then
	    Args = arg(case IdRef of tag(!Local Id) then L.Id
		       [] tag(!Global I) then Closure.(I + 2)
		       [] tag(!Immediate X) then X
		       end)
	 [] tag(!TupArgs IdRefs) then N in
	    N = {Width IdRefs}
	    Args = {MakeTuple args N}
	    for J in 1..N do
	       Args.J = case IdRefs.J of tag(!Local Id) then L.Id
			[] tag(!Global K) then Closure.(K + 2)
			[] tag(!Immediate X) then X
			end
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
	 R0 = case IdRef of tag(!Local Id2) then L.Id2
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref R0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof R then
	    L.Id := {Access R}
	    {Emulate NextInstr Closure L TaskStack}
	 end
      [] tag(!GetTup IdDefs IdRef NextInstr) then T0 in
	 T0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref T0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof T then N in
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
	 T0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref T0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof T then
	    L.Id := T.(I + 1)
	    {Emulate NextInstr Closure L TaskStack}
	 end
      [] tag(!LazySel Id IdRef I NextInstr) then X NewClosure in
	 X = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then Closure.(I + 2)
	     [] tag(!Immediate X) then X
	     end
	 NewClosure = closure(lazySel(LazySelInterpreter) X I)
	 L.Id := transient({NewCell byneed(NewClosure)})
	 {Emulate NextInstr Closure L TaskStack}
      [] tag(!Raise IdRef) then Exn in
	 Exn = case IdRef of tag(!Local Id) then L.Id
	       [] tag(!Global I) then Closure.(I + 2)
	       [] tag(!Immediate X) then X
	       end
	 exception(nil Exn TaskStack)
      [] tag(!Reraise IdRef) then X in
	 X = case IdRef of tag(!Local Id) then L.Id
	     [] tag(!Global I) then Closure.(I + 2)
	     [] tag(!Immediate X) then X
	     end
	 case X of package(Debug Exn) then
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
	 I0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global J) then Closure.(J + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref I0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof I then ThenInstr in
	    ThenInstr = {LitCase 1 {Width IntInstrVec} I IntInstrVec ElseInstr}
	    {Emulate ThenInstr Closure L TaskStack}
	 end
      [] tag(!CompactIntTest IdRef Offset Instrs ElseInstr) then I0 in
	 I0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global J) then Closure.(J + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref I0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof I then Index in
	    Index = I - Offset
	    if Index >= 0 andthen Index < {Width Instrs} then
	       {Emulate Instrs.(Index + 1) Closure L TaskStack}
	    else
	       {Emulate ElseInstr Closure L TaskStack}
	    end
	 end
      [] tag(!RealTest IdRef RealInstrVec ElseInstr) then F0 in
	 F0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref F0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof F then ThenInstr in
	    ThenInstr = {LitCase 1 {Width RealInstrVec}
			 F RealInstrVec ElseInstr}
	    {Emulate ThenInstr Closure L TaskStack}
	 end
      [] tag(!StringTest IdRef StringInstrVec ElseInstr) then S0 in
	 S0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref S0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof S then ThenInstr in
	    ThenInstr = {LitCase 1 {Width StringInstrVec}
			 S StringInstrVec ElseInstr}
	    {Emulate ThenInstr Closure L TaskStack}
	 end
      [] tag(!TagTest IdRef NullaryCases NAryCases ElseInstr) then T0 in
	 T0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref T0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof T then
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
	 T0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref T0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof T then Index in
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
	 C0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
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
		  {Emulate ThenInstr Closure L TaskStack}
	       [] request(Transient) then NewFrame in
		  NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
		  request(Transient args() NewFrame|TaskStack)
	       [] unit then
		  {Emulate ElseInstr Closure L TaskStack}
	       end
	    end
	 end
      [] tag(!VecTest IdRef IdDefsInstrVec ElseInstr) then V0 in
	 V0 = case IdRef of tag(!Local Id) then L.Id
	      [] tag(!Global I) then Closure.(I + 2)
	      [] tag(!Immediate X) then X
	      end
	 case {Deref V0} of Transient=transient(_) then NewFrame in
	    NewFrame = frame(Me tag(TupArgs vector()) Instr Closure L)
	    request(Transient args() NewFrame|TaskStack)
	 elseof V then
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
	    Args = arg(case IdRef of tag(!Local Id) then L.Id
		       [] tag(!Global I) then Closure.(I + 2)
		       [] tag(!Immediate X) then X
		       end)
	 [] tag(!TupArgs IdRefs) then N in
	    N = {Width IdRefs}
	    Args = {MakeTuple args N}
	    for J in 1..N do
	       Args.J = case IdRefs.J of tag(!Local Id) then L.Id
			[] tag(!Global K) then Closure.(K + 2)
			[] tag(!Immediate X) then X
			end
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
	    end
	 end
      abstract: fun {$ function(_ _ _ _ _ _ Transform)} Transform end
      toString:
	 fun {$ Frame}
	    case Frame of frame(_ _ _ closure(Function ...) _) then
	       case {Deref Function} of function(_ F#L#C _ _ _ _ _) then
		  'Alice function '#F#', line '#L#', column '#C
	       end
	    [] handler(_ _ _ _ closure(Function ...) _) then
	       case {Deref Function} of function(_ F#L#C _ _ _ _ _) then
		  'Alice handler '#F#', line '#L#', column '#C
	       end
	    end
	 end)
end
