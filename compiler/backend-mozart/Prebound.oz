%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2000
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
export
   BuiltinTable
   RaiseAliceException
   UnwrapAliceException
define
   proc {RaiseAliceException E Coord}
      {Exception.raiseError alice(E Coord)}
   end

   fun {UnwrapAliceException E}
      case E of error(alice(InnerE ...) ...) then InnerE
      else {Exception.'raise' E} unit
      end
   end

   fun {NumberCompare I J}
      if I == J then 'EQUAL'
      elseif I < J then 'LESS'
      else 'GREATER'
      end
   end

   local
      fun {StringCompareSub S1 S2 L1 L2 N}
	 if N == L1 then
	    if L1 == L2 then 'EQUAL'
	    else 'LESS'
	    end
	 elseif N == L2 then 'GREATER'
	 elsecase {NumberCompare {ByteString.get S1 N} {ByteString.get S2 N}}
	 of 'EQUAL' then {StringCompareSub S1 S2 L1 L2 N + 1}
	 elseof X then X
	 end
      end
   in
      fun {StringCompare S1 S2}
	 {StringCompareSub S1 S2
	  {ByteString.length S1} {ByteString.length S2} 0}
      end
   end

   Hex = hex(&0 &1 &2 &3 &4 &5 &6 &7 &8 &9 &a &b &c &d &e &f)

   fun {ToHex X}
      if X > 15 then {ToHex X div 16} else '' end#Hex.(X mod 16 + 1)
   end

   FutureException = {NewUniqueName 'Future.Future'}

   BuiltinTable =
   builtinTable(
      '=': Value.'=='
      '<>': Value.'\\='
      'Array.array':
	 fun {$ N Init}
	    if 0 =< N andthen N < BuiltinTable.'Array.maxLen' then
	       {Array.new 0 N - 1 Init}
	    else
	       {Exception.raiseError alice(BuiltinTable.'General.Size')}
	       unit
	    end
	 end
      'Array.fromList':
	 fun {$ Xs} N A in
	    N = {Length Xs}
	    A = {Array.new 0 N - 1 unit}
	    {List.forAllInd Xs proc {$ I X} {Array.put A I - 1 X} end}
	    A
	 end
      'Array.length':
	 fun {$ A} {Array.high A} + 1 end
      'Array.maxLen': 0x7FFFFFF
      'Array.sub':
	 fun {$ A I}
	    try
	       {Array.get A I}
	    catch error(kernel(array ...) ...) then
	       {Exception.raiseError alice(BuiltinTable.'General.Subscript')}
	       unit
	    end
	 end
      'Array.update':
	 fun {$ A I X}
	    try
	       {Array.put A I X}
	    catch error(kernel(array ...) ...) then
	       {Exception.raiseError alice(BuiltinTable.'General.Subscript')}
	    end
	    unit
	 end
      'Char.<': Value.'<'
      'Char.>': Value.'>'
      'Char.<=': Value.'=<'
      'Char.>=': Value.'>='
      'Char.ord':
	 fun {$ C} C end
      'Char.chr':
	 fun {$ C}
	    if {Char.is C} then C
	    else
	       {Exception.raiseError alice(BuiltinTable.'General.Chr')}
	       unit
	    end
	 end
      'Char.isAlpha': Char.isAlpha
      'Char.isAlphaNum': Char.isAlNum
      'Char.isCntrl': Char.isCntrl
      'Char.isDigit': Char.isDigit
      'Char.isGraph': Char.isGraph
      'Char.isHexDigit': Char.isXDigit
      'Char.isLower': Char.isLower
      'Char.isPrint': Char.isPrint
      'Char.isPunct': Char.isPunct
      'Char.isSpace': Char.isSpace
      'Char.isUpper': Char.isUpper
      'Char.toLower': Char.toLower
      'Char.toUpper': Char.toUpper
      'Future.Future': FutureException
      'Future.alarm\'':
	 fun {$ X} !!{Alarm (X + 500) div 1000} end
      'Future.await':
	 fun {$ X} {Wait X} X end
      'Future.awaitOne':
	 fun {$ X Y} {WaitOr X Y} X end
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
	 fun {$ P} !!thread {P unit} end end
      'Future.isFailed':
	 fun {$ X}
	    false   %--** unimplemented
	 end
      'Future.isFuture': IsFuture   %--** wrong for failed futures
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
	    case {VirtualString.toString {Value.toVirtualString {Label N} 0 0}}
	    of "<N>" then {ByteString.make "<unknown>"}
	    elseof &<|&N|&:|& |Rest then
	       case {Reverse Rest} of &>|Rest then
		  {ByteString.make {Reverse Rest}}
	       end
	    elseof S then {ByteString.make S}
	    end
	 end
      'GlobalStamp.new':
	 fun {$ unit} {NewName} end
      'GlobalStamp.fromString':
	 fun {$ S} {NewUniqueName {VirtualString.toAtom S}} end
      'GlobalStamp.toString':
	 fun {$ N} {ByteString.make {Value.toVirtualString N 0 0}} end
      'GlobalStamp.compare':
	 fun {$ N1 N2}
	    if N1 == N2 then 'EQUAL'
	    elseif {BootName.'<' N1 N2} then 'LESS'
	    else 'GREATER'
	    end
	 end
      'GlobalStamp.hash': BootName.hash
      'Hole.Cyclic': {NewUniqueName 'Future.Cyclic'}
      'Hole.Hole': {NewUniqueName 'Promise.Promise'}
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
      'Hole.isFailed':
	 fun {$ X}
	    false   %--** unimplemented
	 end
      'Hole.isHole': IsFree
      'Int.~': Number.'~'
      'Int.+': Number.'+'
      'Int.-': Number.'-'
      'Int.*': Number.'*'
      'Int.<': Value.'<'
      'Int.>': Value.'>'
      'Int.<=': Value.'=<'
      'Int.>=': Value.'>='
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
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'General.Div')}
	       unit
	    end
	 end
      'Int.maxInt': 'NONE'
      'Int.minInt': 'NONE'
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
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'General.Div')}
	       unit
	    end
	 end
      'Int.precision': 'NONE'
      'Int.quot':
	 fun {$ X1 X2}
	    try
	       X1 div X2
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'General.Div')}
	       unit
	    end
	 end
      'Int.rem':
	 fun {$ X1 X2}
	    try
	       X1 mod X2
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'General.Div')}
	       unit
	    end
	 end
      'Int.toString':
	 fun {$ I} {ByteString.make {IntToString I}} end
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
      'Real.<': Value.'<'
      'Real.>': Value.'>'
      'Real.<=': Value.'=<'
      'Real.>=': Value.'>='
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
	 fun {$ R}
	    if R >= 0.0 then {Floor R} else {Ceil R} end
	 end
      'Real.rem': Float.'mod'
      'Real.round':
	 fun {$ R}
	    {FloatToInt {Round R}}
	 end
      'Real.toString':
	 fun {$ R}
	    {ByteString.make {FloatToString R}}
	 end
      'Real.trunc':
	 fun {$ R}
	    {FloatToInt if R >= 0.0 then {Floor R} else {Ceil R} end}
	 end
      'Ref.:=':
	 fun {$ R X} {Assign R X} unit end
      'Ref.exchange':
	 fun {$ R X} {Exchange R $ X} end
      'String.^':
	 fun {$ S1 S2} {ByteString.append S1 S2} end
      'String.<':
	 fun {$ S1 S2} {StringCompare S1 S2} == 'LESS' end
      'String.>':
	 fun {$ S1 S2} {StringCompare S1 S2} == 'GREATER' end
      'String.<=':
	 fun {$ S1 S2} {StringCompare S1 S2} \= 'GREATER' end
      'String.>=':
	 fun {$ S1 S2} {StringCompare S1 S2} \= 'LESS' end
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
	       {Exception.raiseError alice(BuiltinTable.'General.Subscript')}
	       unit
	    end
	 end
      'String.substring':
	 fun {$ S I J}
	    try
	       {ByteString.slice S I I + J}
	    catch system(kernel('ByteString.slice' ...) ...) then
	       {Exception.raiseError alice(BuiltinTable.'General.Subscript')}
	       unit
	    end
	 end
      'String.str':
	 fun {$ C} {ByteString.make [C]} end
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
      'Unsafe.Array.sub': Array.get
      'Unsafe.Array.update':
	 fun {$ A I X} {Array.put A I X} unit end
      'Unsafe.String.sub': ByteString.get
      'Unsafe.Vector.sub':
	 fun {$ V I} V.(I + 1) end
      'Unsafe.cast':
	 fun {$ X} X end
      'Vector.fromList':
	 fun {$ Xs} {List.toTuple '#[]' Xs} end
      'Vector.maxLen': 0x7FFFFFF
      'Vector.length': Width
      'Vector.sub':
	 fun {$ V I}
	    try
	       V.(I + 1)
	    catch error(kernel('.' ...) ...) then
	       {Exception.raiseError alice(BuiltinTable.'General.Subscript')}
	       unit
	    end
	 end
      'Vector.tabulate':
	 fun {$ N F} V in
	    try
	       V = {Tuple.make '#[]' N}
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'General.Size')}
	    end
	    {For 1 N 1 proc {$ I} V.I = {F I - 1} end}
	    V
	 end
      'Word.+': BootWord.'+'
      'Word.-': BootWord.'-'
      'Word.*': BootWord.'*'
      'Word.<<': BootWord.'<<'
      'Word.>>': BootWord.'>>'
      'Word.~>>': BootWord.'~>>'
      'Word.andb': BootWord.'andb'
      'Word.div':
	 fun {$ W1 W2}
	    try
	       {BootWord.'div' W1 W2}
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'General.Div')}
	       unit
	    end
	 end
      'Word.fromInt\'': BootWord.make
      'Word.mod':
	 fun {$ W1 W2}
	    try
	       {BootWord.'mod' W1 W2}
	    catch _ then
	       {Exception.raiseError alice(BuiltinTable.'General.Div')}
	       unit
	    end
	 end
      'Word.notb': BootWord.notb
      'Word.orb': BootWord.orb
      'Word.toInt': BootWord.toInt
      'Word.toIntX': BootWord.toIntX
      'Word.toString':
	 fun {$ X} {ByteString.make {ToHex {BootWord.toInt X}}} end
      'Word.wordSize': 31
      'Word.xorb': BootWord.'xorb')
end
