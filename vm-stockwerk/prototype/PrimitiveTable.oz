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
   BootName(newUnique: NewUniqueName '<' hash) at 'x-oz://boot/Name'
   BootFloat(fPow) at 'x-oz://boot/Float'
   BootWord at 'x-oz://boot/Word'
   FloatChunk(toOz fromOz) at 'FloatChunk.so{native}'
   Scheduler(object)
   ByneedInterpreter(interpreter)
export
   ImportOzModule
   Table
define
   NONE = 0
   %SOME = 1

   CONS = 0
   NIL  = 1

   EQUAL   = 0
   GREATER = 1
   LESS    = 2

   BLOCKED    = 0
   RUNNABLE   = 1
   TERMINATED = 2

   fun {C2F S}
      case {ByteString.toString S} of [A B C D E F G H] then
	 {FloatChunk.toOz A B C D E F G H}
      end
   end

   fun {F2C X} A B C D E F G H in
      {FloatChunk.fromOz X ?A ?B ?C ?D ?E ?F ?G ?H}
      {ByteString.make [A B C D E F G H]}
   end

   WordSize = 31

   fun {W2I X}
      {BootWord.toInt X}
   end

   fun {I2W X}
      {BootWord.make WordSize X}
   end

   fun {Deref X}
      case X of transient(TransientState) then
	 case {Access TransientState} of ref(Y) then {Deref Y}
	 else X
	 end
      else X
      end
   end

   fun {IsCyclic X TransientState}
      case {Deref X} of transient(TransientState2) then
	 TransientState2 == TransientState
      else false
      end
   end

   fun {RequestList Xs N}
      case {Deref Xs} of Transient=transient(_) then request(Transient)
      [] tag(!CONS _ Xr) then {RequestList Xr N + 1}
      [] !NIL then N
      end
   end

   fun {RequestListAndElements Xs}
      case {Deref Xs} of Transient=transient(_) then request(Transient)
      [] tag(!CONS X Xr) then
	 case {Deref X} of Transient=transient(_) then request(Transient)
	 else {RequestListAndElements Xr}
	 end
      [] !NIL then continue
      end
   end

   proc {MyForAllInd Xs I F}
      case {Deref Xs} of tag(!CONS X Xr) then
	 {F I X}
	 {MyForAllInd Xr I + 1 F}
      [] !NIL then skip
      end
   end

   fun {ListToOz Xs}
      case {Deref Xs} of tag(!CONS X Xr) then {Deref X}|{ListToOz Xr}
      [] !NIL then nil
      end
   end

   fun {IsFailed X}
      case {Deref X} of transient(TransientState) then
	 case {Access TransientState} of cancelled(_) then 1
	 else 0
	 end
      else 0
      end
   end

   fun {NumberCompare I J}
      if I == J then EQUAL
      elseif I < J then LESS
      else GREATER
      end
   end

   local
      fun {StringCompareSub S1 S2 L1 L2 N}
	 if N == L1 then
	    if L1 == L2 then EQUAL
	    else LESS
	    end
	 elseif N == L2 then GREATER
	 elsecase {NumberCompare {ByteString.get S1 N} {ByteString.get S2 N}}
	 of !EQUAL then {StringCompareSub S1 S2 L1 L2 N + 1}
	 elseof X then X
	 end
      end
   in
      fun {StringCompare S1 S2}
	 {StringCompareSub S1 S2
	  {ByteString.length S1} {ByteString.length S2} 0}
      end
   end

   fun {StringExplode S I N}
      if I == N then NIL
      else tag(CONS {ByteString.get S I} {StringExplode S I + 1 N})
      end
   end

   fun {AllEqual X Y I N}
      if I > N then true
      else {Equals X.I Y.I} andthen {AllEqual X Y I + 1 N}
      end
   end

   fun {Equals X0 Y0}
      case {Deref X0} of Transient=transient(_) then
	 request(Transient)
      elseof X then
	 case {Deref Y0} of Transient=transient(_) then
	    request(Transient)
	 elseof Y then
	    if {IsTuple X} then
	       {IsTuple Y} andthen
	       {Label X} == {Label Y} andthen
	       {Width X} == {Width Y} andthen
	       {AllEqual X Y 1 {Width X}}
	    else X == Y
	    end
	 end
      end
   end

   Primitives =
   primitives('=':
		 fun {$ X Y TaskStack}
		    case {Equals X Y} of request(Transient) then
		       request(Transient args(X Y) TaskStack.2)
		    [] true then continue(arg(1) TaskStack.2)
		    [] false then continue(arg(0) TaskStack.2)
		    end
		 end#ii_t
	      '<>': 
		 fun {$ X Y TaskStack}
		    case {Equals X Y} of request(Transient) then
		       request(Transient args(X Y) TaskStack.2)
		    [] true then continue(arg(0) TaskStack.2)
		    [] false then continue(arg(1) TaskStack.2)
		    end
		 end#ii_t
	      'Array.array':
		 fun {$ N X TaskStack}
		    if 0 =< N andthen N < Table.'Array.maxLen' then
		       continue(arg({Array.new 0 N - 1 X}) TaskStack.2)
		    else exception(nil Table.'General.Size' TaskStack.2)
		    end
		 end#rr_t
	      'Array.fromList':
		 fun {$ Xs TaskStack}
		    case {RequestList Xs 0} of request(Transient) then
		       request(Transient arg(Xs) TaskStack)
		    elseof N then A in
		       A = {Array.new 0 N - 1 unit}
		       {MyForAllInd Xs 0 proc {$ I X} A.I := X end}
		       continue(arg(A) TaskStack.2)
		    end
		 end#i_t
	      'Array.length': fun {$ A} {Array.high A} + 1 end#r_v
	      'Array.maxLen': value(0x7FFFFFF)
	      'Array.sub':
		 fun {$ A I TaskStack}
		    try continue(arg({Array.get A I}) TaskStack.2)
		    catch error(kernel(array ...) ...) then
		       exception(nil Table.'General.Subscript' TaskStack.2)
		    end
		 end#rr_t
	      'Array.update':
		 fun {$ A I X TaskStack}
		    try
		       {Array.put A I X}
		       continue(args() TaskStack.2)
		    catch error(kernel(array ...) ...) then
		       exception(nil Table.'General.Subscript' TaskStack.2)
		    end
		 end#rri_t
	      'Char.<': Value.'<'#rr_b
	      'Char.>': Value.'>'#rr_b
	      'Char.<=': Value.'=<'#rr_b
	      'Char.>=': Value.'>='#rr_b
	      'Char.ord': fun {$ C} C end#r_v
	      'Char.chr':
		 fun {$ C TaskStack}
		    if {Char.is C} then continue(arg(C) TaskStack.2)
		    else exception(nil Table.'General.Chr' TaskStack.2)
		    end
		 end#r_t
	      'Char.isAlpha': Char.isAlpha#r_b
	      'Char.isAlphaNum': Char.isAlNum#r_b
	      'Char.isCntrl': Char.isCntrl#r_b
	      'Char.isDigit': Char.isDigit#r_b
	      'Char.isGraph': Char.isGraph#r_b
	      'Char.isHexDigit': Char.isXDigit#r_b
	      'Char.isLower': Char.isLower#r_b
	      'Char.isPrint': Char.isPrint#r_b
	      'Char.isPunct': Char.isPrint#r_b
	      'Char.isSpace': Char.isSpace#r_b
	      'Char.isUpper': Char.isUpper#r_b
	      'Char.toLower': Char.toLower#r_v
	      'Char.toUpper': Char.toUpper#r_v
	      'Future.Future': {NewUniqueName 'Future.Future'}
	      'Future.alarm\'': missing('Future.alarm\'')   %--**
%		 fun {$ X} !!{Alarm (X + 500) div 1000} end
	      'Future.await': fun {$ X} X end#r_v
	      'Future.awaitOne': missing('Future.awaitOne')   %--**
%		 fun {$ X Y} {WaitOr X Y} X end
	      'Future.byneed':
		 fun {$ Closure}
		    transient({NewCell byneed(Closure)})
		 end#r_v
	      'Future.concur':
		 fun {$ Closure TaskStack} Transient TaskStack in
		    Transient = transient({NewCell future(nil)})
		    TaskStack = [byneedFrame(ByneedInterpreter.interpreter
					     Transient)]
		    {Scheduler.object newThread(Closure args()
						taskStack: TaskStack)}
		    continue(arg(Transient) TaskStack)
		 end#r_t
	      'Future.isFailed': IsFailed#i_v
	      'Future.isFuture':
		 fun {$ X}
		    case {Deref X} of transient(TransientState) then
		       case {Access TransientState} of future(_) then 1
		       else 0
		       end
		    else 0
		    end
		 end#i_v
	      'General.Bind': value({NewUniqueName 'General.Bind'})
	      'General.Chr': value({NewUniqueName 'General.Chr'})
	      'General.Div': value({NewUniqueName 'General.Div'})
	      'General.Domain': value({NewUniqueName 'General.Domain'})
	      'General.Fail': value({NewUniqueName 'General.Fail'})
	      'General.Match': value({NewUniqueName 'General.Match'})
	      'General.Overflow': value({NewUniqueName 'General.Overflow'})
	      'General.Size': value({NewUniqueName 'General.Size'})
	      'General.Span': value({NewUniqueName 'General.Span'})
	      'General.Subscript': value({NewUniqueName 'General.Subscript'})
	      'General.exnName':
		 fun {$ N}
		    case {VirtualString.toString
			  {Value.toVirtualString {Label N} 0 0}}
		    of "<N>" then {ByteString.make "<unknown>"}
		    elseof &<|&N|&:|& |&'|Rest then
		       case {Reverse Rest} of &>|Rest then
			  {ByteString.make {Reverse Rest}}
		       end
		    elseof &<|&N|&:|& |Rest then
		       case {Reverse Rest} of &>|Rest then
			  {ByteString.make {Reverse Rest}}
		       end
		    elseof S then {ByteString.make S}
		    end
		 end#r_v
	      'GlobalStamp.new': fun {$} {NewName} end#n_v
	      'GlobalStamp.fromString':
		 fun {$ S} {NewUniqueName {VirtualString.toAtom S}} end#r_v
	      'GlobalStamp.toString':
		 fun {$ N}
		    {ByteString.make {Value.toVirtualString N 0 0}}
		 end#r_v
	      'GlobalStamp.compare':
		 fun {$ N1 N2}
		    if N1 == N2 then EQUAL
		    elseif {BootName.'<' N1 N2} then LESS
		    else GREATER
		    end
		 end#rr_v
	      'GlobalStamp.hash': fun {$ N} {BootName.hash N} end#r_v
	      'Hole.Cyclic': value({NewUniqueName 'Future.Cyclic'})
	      'Hole.Hole': value({NewUniqueName 'Promise.Promise'})
	      'Hole.fail':
		 fun {$ X Exn TaskStack}
		    case {Deref X} of Transient=transient(TransientState) then
		       case {Access TransientState} of hole(MyFuture) then
			  %--** exception wrapping?
			  NewTransientState = cancelled(Exn)
		       in
			  {Assign TransientState NewTransientState}
			  case MyFuture of noFuture then skip
			  [] transient(TransientState2) then
			     case {Access TransientState2} of future(Ts) then
				for T in Ts do
				   {Scheduler.object wakeup(T)}
				end
				{Assign TransientState2 NewTransientState}
			     end
			  end
			  continue(args() TaskStack.2)
		       else request(Transient args(Transient Exn) TaskStack)
		       end
		    else exception(nil Table.'Hole.Hole' TaskStack.2)
		    end
		 end#ii_t
	      'Hole.fill':
		 fun {$ X Y TaskStack}
		    case {Deref X} of Transient=transient(TransientState) then
		       case {Access TransientState} of hole(MyFuture) then
			  if {IsCyclic Y TransientState} then
			     exception(nil Table.'Hole.Cyclic' TaskStack.2)
			  else
			     NewTransientState = ref(Y)
			  in
			     {Assign TransientState NewTransientState}
			     case MyFuture of noFuture then skip
			     [] transient(TransientState2) then
				case {Access TransientState2}
				of future(Ts) then
				   for T in Ts do
				      {Scheduler.object wakeup(T)}
				   end
				   {Assign TransientState2 NewTransientState}
				end
			     end
			     continue(args() TaskStack.2)
			  end
		       else request(Transient args(Transient Y) TaskStack)
		       end
		    else exception(nil Table.'Hole.Hole' TaskStack.2)
		    end
		 end#ii_t
	      'Hole.future':
		 fun {$ X TaskStack}
		    case {Deref X} of Transient=transient(TransientState) then
		       case {Access TransientState} of hole(MyFuture) then
			  Res = case MyFuture of noFuture then NewFuture in
				   NewFuture = transient({NewCell future(nil)})
				   {Assign TransientState hole(NewFuture)}
				   NewFuture
				else MyFuture
				end
		       in
			  continue(arg(Res) TaskStack.2)
		       else request(Transient arg(Transient) TaskStack)
		       end
		    else exception(nil Table.'Hole.Hole' TaskStack.2)
		    end
		 end#i_t
	      'Hole.hole': fun {$} transient({NewCell hole(noFuture)}) end#n_v
	      'Hole.isFailed': IsFailed#i_v
	      'Hole.isHole':
		 fun {$ X}
		    case {Deref X} of transient(TransientState) then
		       case {Access TransientState} of hole(_) then 1
		       else 0
		       end
		    else 0
		    end
		 end#i_v
	      'Int.~': Number.'~'#r_v
	      'Int.+': Number.'+'#rr_v
	      'Int.-': Number.'-'#rr_v
	      'Int.*': Number.'*'#rr_v
	      'Int.<': Value.'<'#rr_b
	      'Int.>': Value.'>'#rr_b
	      'Int.<=': Value.'=<'#rr_b
	      'Int.>=': Value.'>='#rr_b
	      'Int.abs': Abs#r_v
	      'Int.compare': NumberCompare#rr_v
	      'Int.div':
		 fun {$ X1 X2 TaskStack}
		    try B1 B2 in
		       B1 = {Int.isNat X1}
		       B2 = {Int.isNat X2}
		       continue(arg(if B1 == B2 then
				       X1 div X2
				    elseif B2 then
				       (X1 - X2 + 1) div X2
				    else
				       (X1 - X2 - 1) div X2
				    end) TaskStack.2)
		    catch _ then
		       exception(nil Table.'General.Div' TaskStack.2)
		    end
		 end#rr_t
	      'Int.maxInt': value(NONE)
	      'Int.minInt': value(NONE)
	      'Int.mod':
		 fun {$ X1 X2 TaskStack}
		    try A in
		       A = X1 mod X2
		       continue(arg(if A == 0 then A
				    elseif A < 0 then
				       if X2 =< 0 then A
				       else A + X2
				       end
				    else   % A > 0
				       if X2 < 0 then A + X2
				       else A
				       end
				    end) TaskStack.2)
		    catch _ then
		       exception(nil Table.'General.Div' TaskStack.2)
		    end
		 end#rr_t
	      'Int.precision': value(NONE)
	      'Int.quot':
		 fun {$ I J TaskStack}
		    try continue(arg(I div J) TaskStack.2)
		    catch _ then
		       exception(nil Table.'General.Div' TaskStack.2)
		    end
		 end#rr_t
	      'Int.rem':
		 fun {$ I J TaskStack}
		    try continue(arg(I mod J) TaskStack.2)
		    catch _ then
		       exception(nil Table.'General.Div' TaskStack.2)
		    end
		 end#rr_t
	      'LargeWord.wordSize': value(32)
	      'List.Empty': value({NewUniqueName 'List.Empty'})
	      'Math.acos': fun {$ X} {F2C {Acos {C2F X}}} end#r_v
	      'Math.acosh': fun {$ X} {F2C {Float.acosh {C2F X}}} end#r_v
	      'Math.asin': fun {$ X} {F2C {Asin {C2F X}}} end#r_v
	      'Math.asinh': fun {$ X} {F2C {Float.asinh {C2F X}}} end#r_v
	      'Math.atan': fun {$ X} {F2C {Atan {C2F X}}} end#r_v
	      'Math.atanh': fun {$ X} {F2C {Float.atanh {C2F X}}} end#r_v
	      'Math.atan2': fun {$ X Y} {F2C {Atan2 {C2F X} {C2F Y}}} end#rr_v
	      'Math.cos': fun {$ X} {F2C {Cos {C2F X}}} end#r_v
	      'Math.cosh': fun {$ X} {F2C {Float.cosh {C2F X}}} end#r_v
	      'Math.e': value({F2C 2.71828182846})
	      'Math.exp': fun {$ X} {F2C {Exp {C2F X}}} end#r_v
	      'Math.ln': fun {$ X} {F2C {Log {C2F X}}} end#r_v
	      'Math.pi': value({F2C 3.14159265359})
	      'Math.pow':
		 fun {$ X Y} {F2C {BootFloat.fPow {C2F X} {C2F Y}}} end#rr_v
	      'Math.sin': fun {$ X} {F2C {Sin {C2F X}}} end#r_v
	      'Math.sinh': fun {$ X} {F2C {Float.sinh {C2F X}}} end#r_v
	      'Math.sqrt': fun {$ X} {F2C {Sqrt {C2F X}}} end#r_v
	      'Math.tan': fun {$ X} {F2C {Tan {C2F X}}} end#r_v
	      'Math.tanh': fun {$ X} {F2C {Float.tanh {C2F X}}} end#r_v
	      'Option.Option': value({NewUniqueName 'Option.Option'})
	      'Real.~': fun {$ X} {F2C ~{C2F X}} end#r_v
	      'Real.+': fun {$ X Y} {F2C {C2F X} + {C2F Y}} end#rr_v
	      'Real.-': fun {$ X Y} {F2C {C2F X} - {C2F Y}} end#rr_v
	      'Real.*': fun {$ X Y} {F2C {C2F X} * {C2F Y}} end#rr_v
	      'Real./': fun {$ X Y} {F2C {C2F X} / {C2F Y}} end#rr_v
	      'Real.<':
		 fun {$ X Y} if {C2F X} < {C2F Y} then 1 else 0 end end#rr_v
	      'Real.>':
		 fun {$ X Y} if {C2F X} > {C2F Y} then 1 else 0 end end#rr_v
	      'Real.<=':
		 fun {$ X Y} if {C2F X} =< {C2F Y} then 1 else 0 end end#rr_v
	      'Real.>=':
		 fun {$ X Y} if {C2F X} >= {C2F Y} then 1 else 0 end end#rr_v
	      'Real.ceil': fun {$ R} {FloatToInt {Ceil {C2F R}}} end#r_v
	      'Real.compare':
		 fun {$ X Y} {NumberCompare {C2F X} {C2F Y}} end#rr_v
	      'Real.floor': fun {$ R} {FloatToInt {Floor {C2F R}}} end#r_v
	      'Real.fromInt': fun {$ X} {F2C {IntToFloat X}} end#r_v
	      'Real.precision': value(52)
	      'Real.realCeil': fun {$ X} {F2C {Ceil {C2F X}}} end#r_v
	      'Real.realFloor': fun {$ X} {F2C {Floor {C2F X}}} end#r_v
	      'Real.realRound': fun {$ X} {F2C {Round {C2F X}}} end#r_v
	      'Real.realTrunc':
		 fun {$ R0} R = {C2F R0} in
		    {F2C if R >= 0.0 then {Floor R} else {Ceil R} end}
		 end#r_v
	      'Real.rem':
		 fun {$ X Y} {F2C {Float.'mod' {C2F X} {C2F Y}}} end#rr_v
	      'Real.round': fun {$ R} {FloatToInt {Round {C2F R}}} end#r_v
	      'Real.toString':
		 fun {$ R} {ByteString.make {FloatToString {C2F R}}} end#r_v
	      'Real.trunc':
		 fun {$ R0} R = {C2F R0} in
		    {FloatToInt if R >= 0.0 then {Floor R} else {Ceil R} end}
		 end#r_v
	      'Ref.:=': fun {$ R X} {Assign R X} tuple() end#ri_v
	      'Ref.exchange': fun {$ R X} {Exchange R $ X} end#ri_v
	      'String.^': ByteString.append#rr_v
	      'String.<':
		 fun {$ S1 S2}
		    if {StringCompare S1 S2} == LESS then 1 else 0 end
		 end#rr_v
	      'String.>':
		 fun {$ S1 S2}
		    if {StringCompare S1 S2} == GREATER then 1 else 0 end
		 end#rr_v
	      'String.<=':
		 fun {$ S1 S2}
		    if {StringCompare S1 S2} \= GREATER then 1 else 0 end
		 end#rr_v
	      'String.>=':
		 fun {$ S1 S2}
		    if {StringCompare S1 S2} \= LESS then 1 else 0 end
		 end#rr_v
	      'String.compare': StringCompare#rr_v
	      'String.explode':
		 fun {$ S} {StringExplode S 0 {ByteString.length S}} end#r_v
	      'String.implode':
		 fun {$ Cs TaskStack}
		    case {RequestListAndElements Cs}
		    of request(Transient) then
		       request(Transient arg(Cs) TaskStack)
		    [] continue then
		       continue(arg({ByteString.make {ListToOz Cs}})
				TaskStack.2)
		    end
		 end#i_t
	      'String.maxSize': value(0x7FFFFFFF)
	      'String.size': ByteString.length#r_v
	      'String.sub':
		 fun {$ S I TaskStack}
		    try
		       continue(arg({ByteString.get S I}) TaskStack.2)
		    catch system(kernel('ByteString.get' ...) ...) then
		       exception(nil Table.'General.Subscript' TaskStack.2)
		    end
		 end#rr_t
	      'String.substring':
		 fun {$ S I J TaskStack}
		    try
		       continue(arg({ByteString.slice S I I + J}) TaskStack.2)
		    catch system(kernel('ByteString.slice' ...) ...) then
		       exception(nil Table.'General.Subscript' TaskStack.2)
		    end
		 end#rrr_t
	      'String.str': fun {$ C} {ByteString.make [C]} end#r_v
	      'Thread.Terminate': value({NewUniqueName 'Thread.Terminate'})
	      'Thread.Terminated': value({NewUniqueName 'Thread.Terminated'})
	      'Thread.current':
		 fun {$} {Scheduler.object getCurrentThread($)} end#n_v
	      'Thread.isSuspended': fun {$ T} {T isSuspended($)} end#r_b
	      'Thread.raiseIn':
		 fun {$ T Exn TaskStack}
		    if T == {Scheduler.object getCurrentThread($)} then
		       exception(nil Exn TaskStack.2)
		    elsecase {T getState($)} of terminated then
		       exception(nil Table.'Thread.Terminated' TaskStack.2)
		    elseof State then OtherTaskStack NewFrame in
		       {T getTaskStack(?OtherTaskStack)}
		       NewFrame = raiseIn(ThreadRaiseInInterpreter Exn)
		       {T setArgsAndTaskStack(args() NewFrame|OtherTaskStack)}
		       case State of blocked then
			  {Scheduler.object wakeup(T)}
		       [] runnable then skip
		       end
		       continue(args() TaskStack.2)
		    end
		 end#ri_t
	      'Thread.resume':
		 fun {$ T}
		    {T setSuspend(false)}
		    {Scheduler.object condEnqueue(T)}
		    tuple()
		 end#r_v
	      'Thread.state':
		 fun {$ T}
		    case {T getState($)} of runnable then RUNNABLE
		    [] blocked then BLOCKED
		    [] terminated then TERMINATED
		    end
		 end#r_v
	      'Thread.suspend':
		 fun {$ T TaskStack}
		    {T setSuspend(true)}
		    if T == {Scheduler.object getCurrentThread($)} then
		       preempt(args() TaskStack.2)
		    else
		       continue(args() TaskStack.2)
		    end
		 end#r_t
	      'Thread.yield':
		 %--** thread argument
		 fun {$ TaskStack} preempt(args() TaskStack.2) end#n_t
	      'Unsafe.Array.sub': Array.get#rr_v
	      'Unsafe.Array.update':
		 fun {$ A I X} {Array.put A I X} tuple() end#rrr_v
	      'Unsafe.String.sub': ByteString.get#rr_v
	      'Unsafe.Vector.sub': fun {$ V I} V.(I + 1) end#rr_v
	      'Unsafe.cast': fun {$ X} X end#i_v
	      'Stockwerk.realToVector':
		 fun {$ S}
		    {List.toTuple vector {ByteString.toString S}}
		 end#r_v
	      'Vector.fromList':
		 fun {$ Xs TaskStack}
		    case {RequestList Xs 0} of request(Transient) then
		       request(Transient arg(Xs) TaskStack)
		    elseof N then V in
		       V = {MakeTuple vector N}
		       {MyForAllInd Xs 1 proc {$ I X} V.I = X end}
		       continue(arg(V) TaskStack.2)
		    end
		 end#i_t
	      'Vector.maxLen': value(0x7FFFFFF)
	      'Vector.length': Width#r_v
	      'Vector.sub':
		 fun {$ V I TaskStack}
		    try continue(arg(V.(I + 1)) TaskStack.2)
		    catch error(kernel('.' ...) ...) then
		       exception(nil Table.'General.Subscript' TaskStack.2)
		    end
		 end#rr_t
	      'Vector.tabulate':
		 fun {$ N F TaskStack}
		    if N > 0 then V NewFrame in
		       V = {Tuple.make vector N}
		       NewFrame = vectorTabulate(VectorTabulateInterpreter
						 V F 1 N)
		       continue(arg(0) {F.1.1.pushCall F NewFrame|TaskStack.2})
		    else
		       continue(arg(vector()) TaskStack.2)
		    end
		 end#rr_t
	      'Word.+':
		 fun {$ X Y} {W2I {BootWord.'+' {I2W X} {I2W Y}}} end#rr_v
	      'Word.-':
		 fun {$ X Y} {W2I {BootWord.'-' {I2W X} {I2W Y}}} end#rr_v
	      'Word.*':
		 fun {$ X Y} {W2I {BootWord.'*' {I2W X} {I2W Y}}} end#rr_v
	      'Word.<<':
		 fun {$ X Y} {W2I {BootWord.'<<' {I2W X} {I2W Y}}} end#rr_v
	      'Word.>>':
		 fun {$ X Y} {W2I {BootWord.'>>' {I2W X} {I2W Y}}} end#rr_v
	      'Word.~>>':
		 fun {$ X Y} {W2I {BootWord.'~>>' {I2W X} {I2W Y}}} end#rr_v
	      'Word.<':
		 fun {$ W1 W2}
		    if {BootWord.'<' {I2W W1} {I2W W2}} then 1 else 0 end
		 end#rr_v
	      'Word.>':
		 fun {$ W1 W2}
		    if {BootWord.'>' {I2W W1} {I2W W2}} then 1 else 0 end
		 end#rr_v
	      'Word.<=':
		 fun {$ W1 W2}
		    if {BootWord.'<=' {I2W W1} {I2W W2}} then 1 else 0 end
		 end#rr_v
	      'Word.>=':
		 fun {$ W1 W2}
		    if {BootWord.'>=' {I2W W1} {I2W W2}} then 1 else 0 end
		 end#rr_v
	      'Word.andb':
		 fun {$ X Y} {W2I {BootWord.'andb' {I2W X} {I2W Y}}} end#rr_v
	      'Word.div':
		 fun {$ W1 W2 TaskStack}
		    try
		       continue(arg({W2I {BootWord.'div' {I2W W1} {I2W W2}}})
				TaskStack.2)
		    catch _ then
		       exception(nil Table.'General.Div' TaskStack.2)
		    end
		 end#rr_t
	      'Word.fromInt\'': fun {$ _ X} X end#rr_v   %--** size
	      'Word.fromWord\'':
		 fun {$ _ X} X end#rr_v   %--** size
	      'Word.fromWordX\'':
		 fun {$ _ X} X end#rr_v   %--** size
	      'Word.mod':
		 fun {$ W1 W2 TaskStack}
		    try
		       continue(arg({W2I {BootWord.'mod' {I2W W1} {I2W W2}}})
				TaskStack.2)
		    catch _ then
		       exception(nil Table.'General.Div' TaskStack.2)
		    end
		 end#rr_t
	      'Word.notb': fun {$ X} {W2I {BootWord.notb {I2W X}}} end#r_v
	      'Word.orb':
		 fun {$ X Y} {W2I {BootWord.orb {I2W X} {I2W Y}}} end#rr_v
	      'Word.toInt': fun {$ X} X end#r_v
	      'Word.toIntX': fun {$ X} {BootWord.toIntX {I2W X}} end#r_v
	      'Word.wordSize': value(31)
	      'Word.xorb':
		 fun {$ X Y} {W2I {BootWord.'xorb' {I2W X} {I2W Y}}} end#rr_v)

   fun {Construct Args}
      case Args of arg(X) then X
      [] args(...) then Args
      end
   end

   fun {Deconstruct Args}
      case Args of arg(X) then {Deref X}
      [] args(...) then Args
      end
   end

   fun {VectorTabulateInterpreterRun Args TaskStack}
      case TaskStack of vectorTabulate(_ V F I N)|Rest then
	 V.I = {Construct Args}
	 if I == N then continue(arg(V) Rest)
	 else NewFrame in
	    NewFrame = vectorTabulate(VectorTabulateInterpreter V F I + 1 N)
	    continue(arg(I) {F.1.1.pushCall F NewFrame|Rest})
	 end
      end
   end

   VectorTabulateInterpreter =
   vectorTabulateInterpreter(run: VectorTabulateInterpreterRun
			     handle:
				fun {$ Debug Exn TaskStack}
				   case TaskStack of Frame|Rest then
				      exception(Frame|Debug Exn Rest)
				   end
				end)

   ThreadRaiseInInterpreter =
   threadRaiseInInterpreter(run:
			       fun {$ _ TaskStack}
				  case TaskStack of raiseIn(_ Exn)|Rest then
				     exception(nil Exn Rest)
				  end
			       end
			    handle:
				fun {$ Debug Exn TaskStack}
				   case TaskStack of Frame|Rest then
				      exception(Frame|Debug Exn Rest)
				   end
				end)

   fun {PrimitiveInterpreterRun Args TaskStack}
      case TaskStack of primitive(_ F Spec _)|Rest then
	 case Spec of n_t then {F TaskStack}
	 [] n_v then continue(arg({F}) Rest)
	 [] i_t then {F {Construct Args} TaskStack}
	 [] i_v then continue(arg({F {Construct Args}}) Rest)
	 [] r_t then X = {Construct Args} in
	    case {Deref X} of Transient=transient(_) then
	       request(Transient arg(Transient) TaskStack)
	    elseof Y then {F Y TaskStack}
	    end
	 [] r_v then X = {Construct Args} in
	    case {Deref X} of Transient=transient(_) then
	       request(Transient arg(Transient) TaskStack)
	    elseof Y then continue(arg({F Y}) Rest)
	    end
	 [] r_b then X = {Construct Args} in
	    case {Deref X} of Transient=transient(_) then
	       request(Transient arg(Transient) TaskStack)
	    elseof Y then continue(arg(if {F Y} then 1 else 0 end) Rest)
	    end
	 [] ii_t then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then {F T.1 T.2 TaskStack}
	    end
	 [] ii_v then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then continue(arg({F T.1 T.2}) Rest)
	    end
	 [] ir_t then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.2} of Transient=transient(_) then
		  request(Transient args(T.1 Transient) TaskStack)
	       elseof T2 then {F T.1 T2 TaskStack}
	       end
	    end
	 [] ri_t then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2) TaskStack)
	       elseof T1 then {F T1 T.2 TaskStack}
	       end
	    end
	 [] ri_v then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2) TaskStack)
	       elseof T1 then continue(arg({F T1 T.2}) Rest)
	       end
	    end
	 [] rr_t then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2) TaskStack)
	       elseof T1 then
		  case {Deref T.2} of Transient=transient(_) then
		     request(Transient args(T1 Transient) TaskStack)
		  elseof T2 then {F T1 T2 TaskStack}
		  end
	       end
	    end
	 [] rr_v then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2) TaskStack)
	       elseof T1 then
		  case {Deref T.2} of Transient=transient(_) then
		     request(Transient args(T1 Transient) TaskStack)
		  elseof T2 then continue(arg({F T1 T2}) Rest)
		  end
	       end
	    end
	 [] rr_b then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2) TaskStack)
	       elseof T1 then
		  case {Deref T.2} of Transient=transient(_) then
		     request(Transient args(T1 Transient) TaskStack)
		  elseof T2 then
		     continue(arg(if {F T1 T2} then 1 else 0 end) Rest)
		  end
	       end
	    end
	 [] rri_t then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2 T.3) TaskStack)
	       elseof T1 then
		  case {Deref T.2} of Transient=transient(_) then
		     request(Transient args(T1 Transient T.3) TaskStack)
		  elseof T2 then {F T1 T2 T.3 TaskStack}
		  end
	       end
	    end
	 [] rri_v then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2 T.3) TaskStack)
	       elseof T1 then
		  case {Deref T.2} of Transient=transient(_) then
		     request(Transient args(T1 Transient T.3) TaskStack)
		  elseof T2 then continue(arg({F T1 T2 T.3}) Rest)
		  end
	       end
	    end
	 [] rrr_t then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2 T.3) TaskStack)
	       elseof T1 then
		  case {Deref T.2} of Transient=transient(_) then
		     request(Transient args(T1 Transient T.3) TaskStack)
		  elseof T2 then
		     case {Deref T.3} of Transient=transient(_) then
			request(Transient args(T1 T2 Transient) TaskStack)
		     elseof T3 then {F T1 T2 T3 TaskStack}
		     end
		  end
	       end
	    end
	 [] rrr_v then
	    case {Deconstruct Args} of Transient=transient(_) then
	       request(Transient Args TaskStack)
	    elseof T then
	       case {Deref T.1} of Transient=transient(_) then
		  request(Transient args(Transient T.2 T.3) TaskStack)
	       elseof T1 then
		  case {Deref T.2} of Transient=transient(_) then
		     request(Transient args(T1 Transient T.3) TaskStack)
		  elseof T2 then
		     case {Deref T.3} of Transient=transient(_) then
			request(Transient args(T1 T2 Transient) TaskStack)
		     elseof T3 then continue(arg({F T1 T2 T3}) Rest)
		     end
		  end
	       end
	    end
	 end
      end
   end

   AlicePrimitive = {ByteString.make 'Alice.primitive'}

   Interpreter =
   primitiveInterpreter(run: PrimitiveInterpreterRun
			handle:
			   fun {$ Debug Exn TaskStack}
			      case TaskStack of Frame|Rest then
				 exception(Frame|Debug Exn Rest)
			      end
			   end
			pushCall:
			   fun {$ Closure TaskStack}
			      case Closure of closure(P=primitive(_ _ _ _))
			      then P|TaskStack
			      end
			   end
			abstract:
			   fun {$ primitive(_ _ _ F)}
			      %--** how to signal sitedness?
			      transform(AlicePrimitive {ByteString.make F})
			   end)

   fun {ImportField F X}
      case X of P#Spec then closure(primitive(Interpreter P Spec F))
      [] value(Y) then Y   %--** cannot be abstracted again
      [] missing(_) then {Value.byNeedFail X}   %--**
      else {ImportOzModule X}
      end
   end

   fun {ImportOzModule Module}
      {Record.mapInd Module ImportField}
   end

   Table = {ImportOzModule Primitives}
end
