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
   table: Primitives
require
   BootName(newUnique: NewUniqueName '<' hash) at 'x-oz://boot/Name'
   BootFloat(fPow) at 'x-oz://boot/Float'
   BootWord at 'x-oz://boot/Word'
prepare
   NONE = 0
   %SOME = 1

   EQUAL   = 0
   GREATER = 1
   LESS    = 2

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
      fun {StringCompare Args} S1 = Args.1 S2 = Args.2 in
	 {StringCompareSub S1 S2
	  {ByteString.length S1} {ByteString.length S2} 0}
      end
   end

   FutureException = {NewUniqueName 'Future.Future'}

   Primitives =
   primitives('=':
		 fun {$ Args} if Args.1 == Args.2 then 1 else 0 end end   %--**
	      '<>':
		 fun {$ Args} if Args.1 \= Args.2 then 1 else 0 end end   %--**
	      'Array.array':
		 fun {$ Args} N = Args.1 in
		    if 0 =< N andthen N < Primitives.'Array.maxLen' then
		       {Array.new 0 N - 1 Args.2}
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
		 fun {$ Args}
		    try {Array.get Args.1 Args.2}
		    catch error(kernel(array ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'Array.update':
		 fun {$ Args}
		    try {Array.put Args.1 Args.2 Args.3} tuple
		    catch error(kernel(array ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'Char.<': fun {$ Args} if Args.1 < Args.2 then 1 else 0 end end
	      'Char.>': fun {$ Args} if Args.1 > Args.2 then 1 else 0 end end
	      'Char.<=': fun {$ Args} if Args.1 =< Args.2 then 1 else 0 end end
	      'Char.>=': fun {$ Args} if Args.1 >= Args.2 then 1 else 0 end end
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
	      'Future.Future': FutureException
	      'Future.alarm\'':
		 fun {$ X} !!{Alarm (X + 500) div 1000} end
	      'Future.await':
		 fun {$ X} {Wait X} X end
	      'Future.awaitOne':
		 fun {$ Args} X = Args.1 in {WaitOr X Args.2} X end
/*--**
      'Future.byneed':
	 fun {$ P}
	    {ByNeed fun {$}
		       try
			  {P unit}
		       catch error(AliceE=alice(InnerE ...) ...) then
			  {Value.byNeedFail
			   error({AdjoinAt AliceE 1 FutureException(InnerE)})}
		       [] error(InnerE ...) then
			  {Value.byNeedFail error(FutureException(InnerE))}
		       end
		    end}
	 end
      'Future.concur':
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
	      'Future.isFailed': fun {$ X} 0 end   %--** unimplemented
	      'Future.isFuture':
		 fun {$ X}
		    if {IsFuture X} then 1 else 0 end
		 end   %--** wrong for failed futures
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
/*--**
      'General.exnName':
	 fun {$ N}
	    case {VirtualString.toString {Value.toVirtualString {Label N} 0 0}}
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
*/
	      'GlobalStamp.new': fun {$ _} {NewName} end
	      'GlobalStamp.fromString':
		 fun {$ S} {NewUniqueName {VirtualString.toAtom S}} end
	      'GlobalStamp.toString':
		 fun {$ N} {ByteString.make {Value.toVirtualString N 0 0}} end
	      'GlobalStamp.compare':
		 fun {$ Args} N1 = Args.1 N2 = Args.2 in
		    if N1 == N2 then EQUAL
		    elseif {BootName.'<' N1 N2} then LESS
		    else GREATER
		    end
		 end
	      'GlobalStamp.hash': BootName.hash
	      'Hole.Cyclic': {NewUniqueName 'Future.Cyclic'}
	      'Hole.Hole': {NewUniqueName 'Promise.Promise'}
/*--**
      'Hole.fail':
	 fun {$ X E}
	    try
	       X = {Value.byNeedFail error(alice(FutureException(E)))}
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'Hole.Hole')}
	    end
	    unit
	 end
      'Hole.fill':
	 fun {$ X Y}
	    if {IsDet X} then   %--** test and bind must be atomic
	       {Exception.raiseError alice(BuiltinTable.'Hole.Hole')}
	    end
	    try
	       X = Y
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'Hole.Hole')}
	    end
	    unit
	 end
      'Hole.future':
	 fun {$ X}
	    if {IsFuture X} then
	       skip   %--** wait until it is bound to a hole
	    end
	    !!X
	 end
      'Hole.hole':
	 fun {$ unit} _ end
*/
	      'Hole.isFailed': fun {$ X} 0 end   %--** unimplemented
	      'Hole.isHole': fun {$ X} if {IsFree X} then 1 else 0 end end
	      'Int.~': Number.'~'
	      'Int.+': fun {$ Args} Args.1 + Args.2 end
	      'Int.-': fun {$ Args} Args.1 - Args.2 end
	      'Int.*': fun {$ Args} Args.1 * Args.2 end
	      'Int.<': fun {$ Args} if Args.1 < Args.2 then 1 else 0 end end
	      'Int.>': fun {$ Args} if Args.1 > Args.2 then 1 else 0 end end
	      'Int.<=': fun {$ Args} if Args.1 =< Args.2 then 1 else 0 end end
	      'Int.>=': fun {$ Args} if Args.1 >= Args.2 then 1 else 0 end end
	      'Int.abs': Abs
	      'Int.compare': fun {$ Args} {NumberCompare Args.1 Args.2} end
	      'Int.div':
		 fun {$ Args} X1 = Args.1 X2 = Args.2 in
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
		 fun {$ Args} X1 = Args.1 X2 = Args.2 in
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
		 fun {$ Args}
		    try
		       Args.1 div Args.2
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'Int.rem':
		 fun {$ Args}
		    try
		       Args.1 mod Args.2
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
	      'Math.atan2': fun {$ Args} {Atan2 Args.1 Args.2} end
	      'Math.cos': Cos
	      'Math.cosh': Float.cosh
	      'Math.e': 2.71828182846
	      'Math.exp': Exp
	      'Math.ln': Log
	      'Math.pi': 3.14159265359
	      'Math.pow': fun {$ Args} {BootFloat.fPow Args.1 Args.2} end
	      'Math.sin': Sin
	      'Math.sinh': Float.sinh
	      'Math.sqrt': Sqrt
	      'Math.tan': Tan
	      'Math.tanh': Float.tanh
	      'Option.Option': {NewUniqueName 'Option.Option'}
	      'Real.~': Number.'~'
	      'Real.+': fun {$ Args} Args.1 + Args.2 end
	      'Real.-': fun {$ Args} Args.1 - Args.2 end
	      'Real.*': fun {$ Args} Args.1 * Args.2 end
	      'Real.<': fun {$ Args} if Args.1 < Args.2 then 1 else 0 end end
	      'Real.>': fun {$ Args} if Args.1 > Args.2 then 1 else 0 end end
	      'Real.<=': fun {$ Args} if Args.1 =< Args.2 then 1 else 0 end end
	      'Real.>=': fun {$ Args} if Args.1 >= Args.2 then 1 else 0 end end
	      'Real.ceil':
		 fun {$ R}
		    {FloatToInt {Ceil R}}
		 end
	      'Real.compare': fun {$ Args} {NumberCompare Args.1 Args.2} end
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
	      'Real.rem': fun {$ Args} {Float.'mod' Args.1 Args.2} end
	      'Real.round': fun {$ R} {FloatToInt {Round R}} end
	      'Real.toString':
		 fun {$ R} {ByteString.make {FloatToString R}} end
	      'Real.trunc':
		 fun {$ R}
		    {FloatToInt if R >= 0.0 then {Floor R} else {Ceil R} end}
		 end
	      'Ref.:=':
		 fun {$ Args} {Assign Args.1 Args.2} tuple end
	      'Ref.exchange':
		 fun {$ Args} {Exchange Args.1 $ Args.2} end
	      'String.^':
		 fun {$ Args} {ByteString.append Args.1 Args.2} end
	      'String.<':
		 fun {$ Args}
		    if {StringCompare Args} == LESS then 1 else 0 end
		 end
	      'String.>':
		 fun {$ Args}
		    if {StringCompare Args} == GREATER then 1 else 0 end
		 end
	      'String.<=':
		 fun {$ Args}
		    if {StringCompare Args} \= GREATER then 1 else 0 end
		 end
	      'String.>=':
		 fun {$ Args}
		    if {StringCompare Args} \= LESS then 1 else 0 end
		 end
	      'String.compare': StringCompare
	      'String.explode': ByteString.toString
	      'String.implode': ByteString.make
	      'String.maxSize': 0x7FFFFFFF
	      'String.size': ByteString.length
	      'String.sub':
		 fun {$ Args}
		    try
		       {ByteString.get Args.1 Args.2}
		    catch system(kernel('ByteString.get' ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'String.substring':
		 fun {$ Args}
		    try I = Args.2 in
		       {ByteString.slice Args.1 I I + Args.3}
		    catch system(kernel('ByteString.slice' ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
	      'String.str':
		 fun {$ C} {ByteString.make [C]} end
/*--**
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
	      'Unsafe.Array.sub': fun {$ Args} {Array.get Args.1 Args.2} end
	      'Unsafe.Array.update':
		 fun {$ Args} {Array.put Args.1 Args.2 Args.3} tuple end
	      'Unsafe.String.sub':
		 fun {$ Args} {ByteString.get Args.1 Args.2} end
	      'Unsafe.Vector.sub': fun {$ Args} Args.1.(Args.2 + 1) end
	      'Unsafe.cast': fun {$ X} X end
	      'Vector.fromList': fun {$ Xs} {List.toTuple vector Xs} end
	      'Vector.maxLen': 0x7FFFFFF
	      'Vector.length': Width
	      'Vector.sub':
		 fun {$ Args}
		    try
		       Args.1.(Args.2 + 1)
		    catch error(kernel('.' ...) ...) then
		       exception(Primitives.'General.Subscript')
		    end
		 end
/*--**
	      'Vector.tabulate':
		 fun {$ Args} N = Args.1 F = Args.2 V in
		    V = {Tuple.make vector N}
		    {For 1 N 1 proc {$ I} V.I = {F I - 1} end}
		    V
		 end
*/
	      'Word.+': fun {$ Args} {BootWord.'+' Args.1 Args.2} end
	      'Word.-': fun {$ Args} {BootWord.'-' Args.1 Args.2} end
	      'Word.*': fun {$ Args} {BootWord.'*' Args.1 Args.2} end
	      'Word.<<': fun {$ Args} {BootWord.'<<' Args.1 Args.2} end
	      'Word.>>': fun {$ Args} {BootWord.'>>' Args.1 Args.2} end
	      'Word.~>>': fun {$ Args} {BootWord.'~>>' Args.1 Args.2} end
	      'Word.<':
		 fun {$ Args}
		    if {BootWord.'<' Args.1 Args.2} then 1 else 0 end
		 end
	      'Word.>':
		 fun {$ Args}
		    if {BootWord.'>' Args.1 Args.2} then 1 else 0 end
		 end
	      'Word.<=':
		 fun {$ Args}
		    if {BootWord.'<=' Args.1 Args.2} then 1 else 0 end
		 end
	      'Word.>=':
		 fun {$ Args}
		    if {BootWord.'>=' Args.1 Args.2} then 1 else 0 end
		 end
	      'Word.andb': fun {$ Args} {BootWord.'andb' Args.1 Args.2} end
	      'Word.div':
		 fun {$ Args}
		    try
		       {BootWord.'div' Args.1 Args.2}
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'Word.fromInt\'': fun {$ Args} {BootWord.make Args.1 Args.2} end
	      'Word.mod':
		 fun {$ Args}
		    try
		       {BootWord.'mod' Args.1 Args.2}
		    catch _ then exception(Primitives.'General.Div')
		    end
		 end
	      'Word.notb': BootWord.notb
	      'Word.orb': fun {$ Args} {BootWord.orb Args.1 Args.2} end
	      'Word.toInt': BootWord.toInt
	      'Word.toIntX': BootWord.toIntX
	      'Word.wordSize': 31
	      'Word.xorb': fun {$ Args} {BootWord.'xorb' Args.1 Args.2} end)
end
