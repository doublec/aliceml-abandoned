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
   ImportOzModule
   Table
import
   BootName(newUnique: NewUniqueName '<' hash) at 'x-oz://boot/Name'
   BootFloat(fPow) at 'x-oz://boot/Float'
   BootWord at 'x-oz://boot/Word'
   Scheduler(object)
define
   NONE = 0
   %SOME = 1

   EQUAL   = 0
   GREATER = 1
   LESS    = 2

   fun {Deref X}
      case X of transient(TransientState) then
	 case {Access TransientState} of ref(Y) then {Deref Y}
	 else X
	 end
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

   Primitives =
   primitives('=':
		 fun {$ X Y} if X == Y then 1 else 0 end end   %--**
	      '<>':
		 fun {$ X Y} if X \= Y then 1 else 0 end end   %--**
	      'Array.array':
		 fun {$ N X}
		    if 0 =< N andthen N < Primitives.'Array.maxLen' then
		       {Array.new 0 N - 1 X}
		    else exception(Primitives.'General.Size')
		    end
		 end
	      'Array.fromList':
		 fun {$ Xs} N A in
		    N = {Length Xs}
		    A = {Array.new 0 N - 1 unit}
		    for X in Xs I in 0..N - 1 do
		       A.I := X
		    end
		    A
		 end
	      'Array.length': fun {$ A} {Array.high A} + 1 end
	      'Array.maxLen': 0x7FFFFFF
	      'Array.sub':
		 fun {$ A I}
		    try {Array.get A I}
		    catch error(kernel(array ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'Array.update':
		 fun {$ A I X}
		    try {Array.put A I X} tuple
		    catch error(kernel(array ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'Char.<': fun {$ C1 C2} if C1 < C2 then 1 else 0 end end
	      'Char.>': fun {$ C1 C2} if C1 > C2 then 1 else 0 end end
	      'Char.<=': fun {$ C1 C2} if C1 =< C2 then 1 else 0 end end
	      'Char.>=': fun {$ C1 C2} if C1 >= C2 then 1 else 0 end end
	      'Char.ord': fun {$ C} C end
	      'Char.chr':
		 fun {$ C}
		    if {Char.is C} then C
		    else exception(Primitives.'General.Chr')
		    end
		 end
	      'Char.isAlpha':
		 fun {$ C} if {Char.isAlpha C} then 1 else 0 end end
	      'Char.isAlphaNum':
		 fun {$ C} if {Char.isAlNum C} then 1 else 0 end end
	      'Char.isCntrl':
		 fun {$ C} if {Char.isCntrl C} then 1 else 0 end end
	      'Char.isDigit':
		 fun {$ C} if {Char.isDigit C} then 1 else 0 end end
	      'Char.isGraph':
		 fun {$ C} if {Char.isGraph C} then 1 else 0 end end
	      'Char.isHexDigit':
		 fun {$ C} if {Char.isXDigit C} then 1 else 0 end end
	      'Char.isLower':
		 fun {$ C} if {Char.isLower C} then 1 else 0 end end
	      'Char.isPrint':
		 fun {$ C} if {Char.isPrint C} then 1 else 0 end end
	      'Char.isPunct':
		 fun {$ C} if {Char.isPunct C} then 1 else 0 end end
	      'Char.isSpace':
		 fun {$ C} if {Char.isSpace C} then 1 else 0 end end
	      'Char.isUpper':
		 fun {$ C} if {Char.isUpper C} then 1 else 0 end end
	      'Char.toLower': Char.toLower
	      'Char.toUpper': Char.toUpper
	      'Future.Future': {NewUniqueName 'Future.Future'}
	      'Future.alarm\'': missing('Future.alarm\'')   %--**
%		 fun {$ X} !!{Alarm (X + 500) div 1000} end
	      'Future.await':
		 fun {$ X0}
		    case {Deref X0} of Transient=transient(_) then
		       request(Transient)
		    elseof X then X
		    end
		 end
	      'Future.awaitOne': missing('Future.awaitOne')   %--**
%		 fun {$ X Y} {WaitOr X Y} X end
	      'Future.byneed':
		 fun {$ Closure} transient({NewCell byneed(Closure)}) end
	      'Future.concur': missing('Future.concur')   %--**
/*
	      fun {$ P}
		 !!thread
		      try
			 {P unit}
		      catch error(AliceE=alice(InnerE ...) ...) then
			 {Value.byNeedFail
			  error({AdjoinAt AliceE 1 FutureException(InnerE)})}
		      [] error(InnerE ...) then
			 {Value.byNeedFail error(FutureException(InnerE))}
		      end
		   end
	      end
*/
	      'Future.isFailed': IsFailed
	      'Future.isFuture':
		 fun {$ X}
		    case {Deref X} of transient(TransientState) then
		       case {Access TransientState} of future(_) then 1
		       else 0
		       end
		    else 0
		    end
		 end
	      'General.Bind': {NewUniqueName 'General.Bind'}
	      'General.Chr': {NewUniqueName 'General.Chr'}
	      'General.Div': {NewUniqueName 'General.Div'}
	      'General.Domain': {NewUniqueName 'General.Domain'}
	      'General.Fail': {NewUniqueName 'General.Fail'}
	      'General.Match': {NewUniqueName 'General.Match'}
	      'General.Overflow': {NewUniqueName 'General.Overflow'}
	      'General.Size': {NewUniqueName 'General.Size'}
	      'General.Span': {NewUniqueName 'General.Span'}
	      'General.Subscript': {NewUniqueName 'General.Subscript'}
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
		 end
	      'GlobalStamp.new': fun {$ _} {NewName} end
	      'GlobalStamp.fromString':
		 fun {$ S} {NewUniqueName {VirtualString.toAtom S}} end
	      'GlobalStamp.toString':
		 fun {$ N} {ByteString.make {Value.toVirtualString N 0 0}} end
	      'GlobalStamp.compare':
		 fun {$ N1 N2}
		    if N1 == N2 then EQUAL
		    elseif {BootName.'<' N1 N2} then LESS
		    else GREATER
		    end
		 end
	      'GlobalStamp.hash': BootName.hash
	      'Hole.Cyclic': {NewUniqueName 'Future.Cyclic'}
	      'Hole.Hole': {NewUniqueName 'Promise.Promise'}
	      'Hole.fail':
		 fun {$ X Exn}
		    case {Deref X} of Transient=transient(TransientState) then
		       case {Access TransientState} of hole(MyFuture) then
			  %--** exception wrapping
			  NewTransientState = cancelled(Exn)
		       in
			  {Assign TransientState NewTransientState}
			  case MyFuture of noFuture then skip
			  [] transient(TransientState2) then
			     case {Access TransientState2} of future(Ts) then
				for T in Ts do
				   {Scheduler.object enqueue(T)}
				end
				{Assign TransientState2 NewTransientState}
			     end
			  end
			  tuple()
		       else request(Transient)
		       end
		    else exception(Primitives.'Hole.Hole')
		    end
		 end
	      'Hole.fill':
		 fun {$ X Y}
		    %--** cyclic
		    case {Deref X} of Transient=transient(TransientState) then
		       case {Access TransientState} of hole(MyFuture) then
			  NewTransientState = ref(Y)
		       in
			  {Assign TransientState NewTransientState}
			  case MyFuture of noFuture then skip
			  [] transient(TransientState2) then
			     case {Access TransientState2} of future(Ts) then
				for T in Ts do
				   {Scheduler.object enqueue(T)}
				end
				{Assign TransientState2 NewTransientState}
			     end
			  end
			  tuple()
		       else request(Transient)
		       end
		    else exception(Primitives.'Hole.Hole')
		    end
		 end
	      'Hole.future':
		 fun {$ X}
		    case {Deref X} of Transient=transient(TransientState) then
		       case {Access TransientState} of hole(MyFuture) then
			  case MyFuture of noFuture then NewFuture in
			     NewFuture = transient({NewCell future(nil)})
			     {Assign TransientState hole(NewFuture)}
			     NewFuture
			  else MyFuture
			  end
		       else request(Transient)
		       end
		    else exception(Primitives.'Hole.Hole')
		    end
		 end
	      'Hole.hole': fun {$} transient({NewCell hole(noFuture)}) end
	      'Hole.isFailed': IsFailed
	      'Hole.isHole':
		 fun {$ X}
		    case {Deref X} of transient(TransientState) then
		       case {Access TransientState} of hole(_) then 1
		       else 0
		       end
		    else 0
		    end
		 end
	      'Int.~': Number.'~'
	      'Int.+': Number.'+'
	      'Int.-': Number.'-'
	      'Int.*': Number.'*'
	      'Int.<': fun {$ I J} if I < J then 1 else 0 end end
	      'Int.>': fun {$ I J} if I > J then 1 else 0 end end
	      'Int.<=': fun {$ I J} if I =< J then 1 else 0 end end
	      'Int.>=': fun {$ I J} if I >= J then 1 else 0 end end
	      'Int.abs': Abs
	      'Int.compare': NumberCompare
	      'Int.div':
		 fun {$ X1 X2}
		    try B1 B2 in
		       B1 = {Int.isNat X1}
		       B2 = {Int.isNat X2}
		       if B1 == B2 then
			  X1 div X2
		       elseif B2 then
			  (X1 - X2 + 1) div X2
		       else
			  (X1 - X2 - 1) div X2
		       end
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'Int.maxInt': NONE
	      'Int.minInt': NONE
	      'Int.mod':
		 fun {$ X1 X2}
		    try A in
		       A = X1 mod X2
		       if A == 0 then A
		       elseif A < 0 then
			  if X2 =< 0 then A
			  else A + X2
			  end
		       else   % A > 0
			  if X2 < 0 then A + X2
			  else A
			  end
		       end
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'Int.precision': NONE
	      'Int.quot':
		 fun {$ I J}
		    try I div J
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'Int.rem':
		 fun {$ I J}
		    try I mod J
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'List.Empty': {NewUniqueName 'List.Empty'}
	      'Math.acos': Acos
	      'Math.acosh': Float.acosh
	      'Math.asin': Asin
	      'Math.asinh': Float.asinh
	      'Math.atan': Atan
	      'Math.atanh': Float.atanh
	      'Math.atan2': Atan2
	      'Math.cos': Cos
	      'Math.cosh': Float.cosh
	      'Math.e': 2.71828182846
	      'Math.exp': Exp
	      'Math.ln': Log
	      'Math.pi': 3.14159265359
	      'Math.pow': BootFloat.fPow
	      'Math.sin': Sin
	      'Math.sinh': Float.sinh
	      'Math.sqrt': Sqrt
	      'Math.tan': Tan
	      'Math.tanh': Float.tanh
	      'Option.Option': {NewUniqueName 'Option.Option'}
	      'Real.~': Number.'~'
	      'Real.+': Number.'+'
	      'Real.-': Number.'-'
	      'Real.*': Number.'*'
	      'Real./': Float.'/'
	      'Real.<': fun {$ X Y} if X < Y then 1 else 0 end end
	      'Real.>': fun {$ X Y} if X > Y then 1 else 0 end end
	      'Real.<=': fun {$ X Y} if X =< Y then 1 else 0 end end
	      'Real.>=': fun {$ X Y} if X >= Y then 1 else 0 end end
	      'Real.ceil':
		 fun {$ R}
		    {FloatToInt {Ceil R}}
		 end
	      'Real.compare': NumberCompare
	      'Real.floor':
		 fun {$ R}
		    {FloatToInt {Floor R}}
		 end
	      'Real.fromInt': IntToFloat
	      'Real.precision': 52
	      'Real.realCeil': Ceil
	      'Real.realFloor': Floor
	      'Real.realRound': Round
	      'Real.realTrunc':
		 fun {$ R} if R >= 0.0 then {Floor R} else {Ceil R} end end
	      'Real.rem': Float.'mod'
	      'Real.round': fun {$ R} {FloatToInt {Round R}} end
	      'Real.toString':
		 fun {$ R} {ByteString.make {FloatToString R}} end
	      'Real.trunc':
		 fun {$ R}
		    {FloatToInt if R >= 0.0 then {Floor R} else {Ceil R} end}
		 end
	      'Ref.:=':
		 fun {$ R X} {Assign R X} tuple end
	      'Ref.exchange':
		 fun {$ R X} {Exchange R $ X} end
	      'String.^': ByteString.append
	      'String.<':
		 fun {$ S1 S2}
		    if {StringCompare S1 S2} == LESS then 1 else 0 end
		 end
	      'String.>':
		 fun {$ S1 S2}
		    if {StringCompare S1 S2} == GREATER then 1 else 0 end
		 end
	      'String.<=':
		 fun {$ S1 S2}
		    if {StringCompare S1 S2} \= GREATER then 1 else 0 end
		 end
	      'String.>=':
		 fun {$ S1 S2}
		    if {StringCompare S1 S2} \= LESS then 1 else 0 end
		 end
	      'String.compare': StringCompare
	      'String.explode': ByteString.toString
	      'String.implode': ByteString.make
	      'String.maxSize': 0x7FFFFFFF
	      'String.size': ByteString.length
	      'String.sub':
		 fun {$ S I}
		    try
		       {ByteString.get S I}
		    catch system(kernel('ByteString.get' ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'String.substring':
		 fun {$ S I J}
		    try
		       {ByteString.slice S I I + J}
		    catch system(kernel('ByteString.slice' ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'String.str':
		 fun {$ C} {ByteString.make [C]} end
	      'Thread.Terminate': missing('Thread.Terminate')   %--**
	      'Thread.current': missing('Thread.current')   %--**
	      'Thread.isSuspended': missing('Thread.isSuspended')   %--**
	      'Thread.raiseIn': missing('Thread.raiseIn')   %--**
	      'Thread.resume': missing('Thread.resume')   %--**
	      'Thread.state': missing('Thread.state')   %--**
	      'Thread.suspend': missing('Thread.suspend')   %--**
	      'Thread.yield': missing('Thread.yield')   %--**
/*
	      'Thread.Terminate': kernel(terminate)
	      'Thread.current':
		 fun {$ unit} {Thread.this} end
	      'Thread.isSuspended': Thread.isSuspended
	      'Thread.raiseIn':
		 fun {$ T E} {Thread.injectException T E} unit end
	      'Thread.resume':
		 fun {$ T} {Thread.resume T} unit end
	      'Thread.state':
		 fun {$ T}
		    case {Thread.state T} of runnable then 'RUNNABLE'
		    [] blocked then 'BLOCKED'
		    [] terminated then 'TERMINATED'
		    end
		 end
	      'Thread.suspend':
		 fun {$ T} {Thread.suspend T} unit end
	      'Thread.yield':
		 fun {$ T} {Thread.preempt T} unit end
*/
	      'Unsafe.Array.sub': Array.get
	      'Unsafe.Array.update':
		 fun {$ A I X} {Array.put A I X} tuple end
	      'Unsafe.String.sub': ByteString.get
	      'Unsafe.Vector.sub': fun {$ V I} V.(I + 1) end
	      'Unsafe.cast': fun {$ X} X end
	      'Vector.fromList': fun {$ Xs} {List.toTuple vector Xs} end
	      'Vector.maxLen': 0x7FFFFFF
	      'Vector.length': Width
	      'Vector.sub':
		 fun {$ V I}
		    try V.(I + 1)
		    catch error(kernel('.' ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'Vector.tabulate': missing('Vector.tabulate')   %--**
/*
		 fun {$ Args} N = Args.1 F = Args.2 V in
		    V = {Tuple.make vector N}
		    {For 1 N 1 proc {$ I} V.I = {F I - 1} end}
		    V
		 end
*/
	      'Word.+': BootWord.'+'
	      'Word.-': BootWord.'-'
	      'Word.*': BootWord.'*'
	      'Word.<<': BootWord.'<<'
	      'Word.>>': BootWord.'>>'
	      'Word.~>>': BootWord.'~>>'
	      'Word.<':
		 fun {$ W1 W2}
		    if {BootWord.'<' W1 W2} then 1 else 0 end
		 end
	      'Word.>':
		 fun {$ W1 W2}
		    if {BootWord.'>' W1 W2} then 1 else 0 end
		 end
	      'Word.<=':
		 fun {$ W1 W2}
		    if {BootWord.'<=' W1 W2} then 1 else 0 end
		 end
	      'Word.>=':
		 fun {$ W1 W2}
		    if {BootWord.'>=' W1 W2} then 1 else 0 end
		 end
	      'Word.andb': BootWord.'andb'
	      'Word.div':
		 fun {$ W1 W2}
		    try
		       {BootWord.'div' W1 W2}
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'Word.fromInt\'': BootWord.make
	      'Word.mod':
		 fun {$ W1 W2}
		    try {BootWord.'mod' W1 W2}
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'Word.notb': BootWord.notb
	      'Word.orb': BootWord.orb
	      'Word.toInt': BootWord.toInt
	      'Word.toIntX': BootWord.toIntX
	      'Word.wordSize': 31
	      'Word.xorb': BootWord.'xorb')

   fun {Construct Args}
      case Args of arg(X) then X
      [] args(...) then Args
      end
   end

   fun {Deconstruct Args}
      case Args of arg(X) then X
      [] args(...) then Args
      end
   end

   Interpreter =
   primitiveInterpreter(run:
			   fun {$ Args TaskStack}
			      case TaskStack of primitive(_ F)|Rest then
				 Res = case {Procedure.arity F} of 1 then {F}
				       [] 2 then {F {Construct Args}}
				       [] 3 then T = {Deconstruct Args} in
					  {F T.1 T.2}
				       [] 4 then T = {Deconstruct Args} in
					  {F T.1 T.2 T.3}
				       end
			      in
				 case Res of exception(Exn) then
				    exception(nil Exn Rest)
				 [] request(Transient) then
				    request(Transient Args TaskStack)
				 elseof Res then continue(arg(Res) Rest)
				 end
			      end
			   end
			handle:
			   fun {$ Debug Exn TaskStack}
			      case TaskStack of Frame|Rest then
				 exception(Frame|Debug Exn Rest)
			      end
			   end
			pushCall:
			   fun {$ Closure TaskStack}
			      case Closure of closure(P=primitive(_ _)) then
				 P|TaskStack
			      end
			   end)

   fun {ImportOzModule Module}
      {Record.map Module
       fun {$ X}
	  if {IsProcedure X} then closure(primitive(Interpreter X))
	  else X
	  end
       end}
   end

   Table = {ImportOzModule Primitives}
end
