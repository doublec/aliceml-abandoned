%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   BootValue(byNeedFail: ByNeedFail) at 'x-oz://boot/Value'
   BootName(newUnique: NewUniqueName '<' hash) at 'x-oz://boot/Name'
   BootFloat(fPow) at 'x-oz://boot/Float'
   BootWord at 'x-oz://boot/Word'
export
   BuiltinTable
   Env
define
   fun {NumberCompare I J}
      if I == J then 'EQUAL'
      elseif I < J then 'LESS'
      else 'GREATER'
      end
   end

   local
      fun {StringCompareSub S1 S2 L1 L2 N}
	 if N >= L1 then
	    if L1 == L2 then 'EQUAL'
	    else 'LESS'
	    end
	 elseif N >= L2 then 'GREATER'
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

   fun {ToOct C}
      C mod 8 + &0
   end

   Hex = hex(&0 &1 &2 &3 &4 &5 &6 &7 &8 &9 &a &b &c &d &e &f)

   fun {ToHex X}
      if X > 15 then {ToHex X div 16} else '' end#Hex.(X mod 16 + 1)
   end

   ByNeedException = {NewUniqueName 'Transient.ByNeed'}

   BuiltinTable =
   builtinTable(
      '=': Value.'=='
      '<>': Value.'\\='
      'Array.array':
	 fun {$ N Init}
	    if 0 =< N andthen N < BuiltinTable.'Array.maxLen' then
	       {Array.new 0 N - 1 Init}
	    else {Exception.raiseError BuiltinTable.'General.Size'} unit
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
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'} unit
	    end
	 end
      'Array.update':
	 fun {$ A I X}
	    try
	       {Array.put A I X}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'}
	    end
	    unit
	 end
      'Char.<': Value.'<'
      'Char.>': Value.'>'
      'Char.<=': Value.'=<'
      'Char.>=': Value.'>='
      'Char.ord': fun {$ C} C end
      'Char.chr':
	 fun {$ C}
	    if {Char.is C} then C
	    else {Exception.raiseError BuiltinTable.'General.Chr'} unit
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
      'Char.toCString':
	 fun {$ C}
	    {ByteString.make
	     case C of &\\ then "\\\\"
	     [] &" then "\\\""
	     [] &? then "\\?"
	     [] &' then "\\'"
	     [] &\a then "\\a"
	     [] &\b then "\\b"
	     [] &\t then "\\t"
	     [] &\n then "\\n"
	     [] &\v then "\\v"
	     [] &\f then "\\f"
	     [] &\r then "\\r"
	     elseif {Char.isPrint C} then [C]
	     else [&\\ {ToOct C div 64} {ToOct C div 8} {ToOct C}]
	     end}
	 end
      'Char.toLower': Char.toLower
      'Char.toUpper': Char.toUpper
      'General.:=': fun {$ X Y} {Assign X Y} unit end
      'General.Chr': {NewUniqueName 'General.Chr'}
      'General.Div': {NewUniqueName 'General.Div'}
      'General.Domain': {NewUniqueName 'General.Domain'}
      'General.Fail': {NewUniqueName 'General.Fail'}
      'General.Overflow': {NewUniqueName 'General.Overflow'}
      'General.Size': {NewUniqueName 'General.Size'}
      'General.Span': {NewUniqueName 'General.Span'}
      'General.Subscript': {NewUniqueName 'General.Subscript'}
      'General.exchange':
	 fun {$ C New} {Exchange C $ New} end
      'General.exnName':
	 fun {$ N}
	    case {VirtualString.toString {Value.toVirtualString {Label N} 0 0}}
	    of "<N>" then ""
	    elseof &<|&N|&:|& |Rest then
	       case {Reverse Rest} of &>|Rest then
		  {ByteString.make {Reverse Rest}}
	       end
	    end
	 end
      'GlobalStamp.new':
	 fun {$ unit} {NewName} end
      'GlobalStamp.fromString':
	 fun {$ S} {NewUniqueName {VirtualString.toAtom S}} end
      'GlobalStamp.toString':
	 fun {$ N} {ByteString.make '<N>'} end
      'GlobalStamp.compare':
	 fun {$ N1 N2}
	    if N1 == N2 then 'EQUAL'
	    elseif {BootName.'<' N1 N2} then 'LESS'
	    else 'GREATER'
	    end
	 end
      'GlobalStamp.hash': BootName.hash
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
	    try
	       X1 div X2
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Div'} unit
	    end
	 end
      'Int.maxInt': 'NONE'
      'Int.minInt': 'NONE'
      'Int.mod':
	 fun {$ X1 X2}
	    try
	       X1 mod X2
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Div'} unit
	    end
	 end
      'Int.precision': 'NONE'
      'Int.toString':
	 fun {$ I} {ByteString.make {Int.toString I}} end
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
      'Real.negInf': ~1.0 / 0.0
      'Real.posInf': 1.0 / 0.0
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
      'Real.toString': FloatToString
      'Real.trunc':
	 fun {$ R}
	    {FloatToInt if R >= 0.0 then {Floor R} else {Ceil R} end}
	 end
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
      'String.maxSize': 0x7FFFFFFF
      'String.size': ByteString.length
      'String.sub':
	 fun {$ S I}
	    try
	       {ByteString.get S I}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'} unit
	    end
	 end
      'String.substring':
	 fun {$ S I J}
	    try
	       {ByteString.slice S I I + J}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'} unit
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
      'Thread.spawn':
	 fun {$ P} thread {P unit} end end
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
      'Transient.ByNeed': ByNeedException
      'Transient.Fulfill': {NewUniqueName 'Transient.Fulfill'}
      'Transient.Future': {NewUniqueName 'Transient.Future'}
      'Transient.Promise': {NewUniqueName 'Transient.Promise'}
      'Transient.alarm\'':
	 fun {$ X} {Alarm (X + 500) div 1000} end
      'Transient.await':
	 fun {$ X} {Wait X} X end
      'Transient.awaitOne':
	 fun {$ X Y} {WaitOr X Y} X end
      'Transient.byNeed':
	 fun {$ P}
	    {ByNeed fun {$}
		       try
			  {P unit}
		       catch E=error(E2 ...) then
			  {ByNeedFail {AdjoinAt E 1 ByNeedException(E2)}}
		       end
		    end}
	 end
      'Transient.fail':
	 fun {$ P X}
	    try
	       P = {ByNeedFail error(X)}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'Transient.Promise'}
	    end
	    unit
	 end
      'Transient.fulfill':
	 fun {$ P X}
	    if {IsFree P} then
	       P = X
	    else
	       {Exception.raiseError BuiltinTable.'Transient.Fulfill'}
	    end
	    unit
	 end
      'Transient.future':
	 fun {$ P}
	    if {IsFree P} andthen {Not {IsFuture P}} then !!P
	    else
	       {Exception.raiseError BuiltinTable.'Transient.Future'} unit
	    end
	 end
      'Transient.isFuture': IsFuture
      'Transient.isPromise': IsFree
      'Transient.promise':
	 fun {$ unit} _ end
      'Unsafe.cast': fun {$ X} X end
      'Vector.fromList':
	 fun {$ Xs} {List.toTuple '#' Xs} end
      'Vector.maxLen': 0x7FFFFFF
      'Vector.length': Width
      'Vector.sub':
	 fun {$ V I}
	    try
	       V.(I + 1)
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'} unit
	    end
	 end
      'Word.fromInt\'': BootWord.make
      'Word.toInt': BootWord.toInt
      'Word.toIntX': BootWord.toIntX
      'Word.+': BootWord.'+'
      'Word.-': BootWord.'-'
      'Word.*': BootWord.'*'
      'Word.div':
	 fun {$ W1 W2}
	    try
	       {BootWord.'div' W1 W2}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Div'} unit
	    end
	 end
      'Word.mod':
	 fun {$ W1 W2}
	    try
	       {BootWord.'mod' W1 W2}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Div'} unit
	    end
	 end
      'Word.orb': BootWord.'orb'
      'Word.xorb': BootWord.'xorb'
      'Word.andb': BootWord.'andb'
      'Word.notb': BootWord.notb
      'Word.<<': BootWord.'<<'
      'Word.>>': BootWord.'>>'
      'Word.~>>': BootWord.'~>>'
      'Word.toString':
	 fun {$ X} {ByteString.make {ToHex {BootWord.toInt X}}} end
      'Word.wordSize': 31)

   Env = env('false': false
	     'true': true
	     'nil': 'nil'
	     'cons': '|'
	     'ref': NewCell
	     'Match': {NewUniqueName 'General.Match'}
	     'Bind': {NewUniqueName 'General.Bind'})
end
