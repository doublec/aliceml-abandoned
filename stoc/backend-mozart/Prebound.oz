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
   BootName(newUnique: NewUniqueName '<' hash) at 'x-oz://boot/Name'
   BootWord at 'x-oz://boot/Word'
   System(show printInfo)
   Browser(browse)
   Application(exit)
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

   fun {StringLess S1 S2}
      {VirtualString.toAtom S1} < {VirtualString.toAtom S2}   %--** inefficient
   end

   CStringTab =
   cStringTab(&\\: "\\\\"
	      &": "\\\""
	      &?: "\\?"
	      &': "\\'"
	      &\a: "\\a"
	      &\b: "\\b"
	      &\t: "\\t"
	      &\n: "\\n"
	      &\v: "\\v"
	      &\f: "\\f"
	      &\r: "\\r")

   fun {ToOct C}
      C mod 8 + &0
   end

   fun {ToCString C}
      case {CondSelect CStringTab C unit} of unit then
	 if {Char.isPrint C} then [C]
	 else [&\\ {ToOct C div 64} {ToOct C div 8} {ToOct C}]
	 end
      elseof S then S
      end
   end

   Hex = hex(&0 &1 &2 &3 &4 &5 &6 &7 &8 &9 &a &b &c &d &e &f)

   fun {ToHex X}
      if X > 15 then {ToHex X div 16} else '' end#Hex.(X mod 16)
   end

   BuiltinTable =
   builtinTable(
      '=': Value.'=='
      '<>': Value.'\\='
      'Array.array':
	 fun {$ N Init} {Array.new 0 N - 1 Init} end
      'Array.fromList':
	 fun {$ Xs} N A in
	    N = {Length Xs}
	    A = {Array.new 0 N - 1 unit}
	    {List.forAllInd Xs proc {$ I X} {Array.put A I - 1 X} end}
	    A
	 end
      'Array.length':
	 fun {$ A} {Array.high A} + 1 end
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
      'Char.<=': fun {$ X1 X2} X1 =< X2 end
      'Char.ord': fun {$ C} C end
      'Char.chr':
	 fun {$ C}
	    if {Char.is C} then C
	    else {Exception.raiseError BuiltinTable.'General.Chr'} unit
	    end
	 end
      'Char.isAlphaNum': Char.isAlNum
      'Char.isDigit': Char.isDigit
      'Char.isHexDigit': Char.isXDigit
      'Char.isSpace': Char.isSpace
      'Char.toCString':
	 fun {$ C} {ByteString.make {ToCString C}} end
      'Char.toLower': Char.toLower
      'General.:=': fun {$ X Y} {Assign X Y} unit end
      'General.Chr': {NewUniqueName 'General.Chr'}
      'General.Div': {NewUniqueName 'General.Div'}
      'General.Domain': {NewUniqueName 'General.Domain'}
      'General.exchange':
	 fun {$ C New} {Exchange C $ New} end
      'General.Fail': {NewUniqueName 'General.Fail'}
      'General.Overflow': {NewUniqueName 'General.Overflow'}
      'General.Size': {NewUniqueName 'General.Size'}
      'General.Span': {NewUniqueName 'General.Span'}
      'General.Subscript': {NewUniqueName 'General.Subscript'}
      'GlobalStamp.new': {NewName}
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
      'Int.mod':
	 fun {$ X1 X2}
	    try
	       X1 mod X2
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Div'} unit
	    end
	 end
      'Int.toString':
	 fun {$ I} {ByteString.make {Int.toString I}} end
      'List.Empty': {NewUniqueName 'List.Empty'}
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
      'Real.compare': NumberCompare
      'Real.fromInt': IntToFloat
      'Real.trunc':
	 fun {$ R}
	    {FloatToInt if R >= 0.0 then {Floor R} else {Ceil R} end}
	 end
      'String.^':
	 fun {$ S1 S2} {ByteString.append S1 S2} end
      'String.<': StringLess
      'String.>':
	 fun {$ X1 X2} {StringLess X2 X1} end
      'String.<=':
	 fun {$ X1 X2} {Not {StringLess X2 X1}} end
      'String.>=':
	 fun {$ X1 X2} {Not {StringLess X1 X2}} end
      'String.str':
	 fun {$ C} {ByteString.make [C]} end
      'String.size':
	 fun {$ S} {ByteString.length S} end
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
      'String.compare':
	 fun {$ S T}
	    if {StringLess S T} then 'LESS'
	    elseif {StringLess T S} then 'GREATER'
	    else 'EQUAL'
	    end
	 end
      'String.explode':
	 fun {$ S} {ByteString.toString S} end
      'Thread.Terminate': kernel(terminate)
      'Thread.current':
	 fun {$ unit} {Thread.this} end
      'Thread.isSuspended': Thread.isSuspended
      'Thread.raiseIn':
	 fun {$ T E} {Thread.injectException T E} unit end
      'Thread.resume':
	 fun {$ T} {Thread.resume T} unit end
      'Thread.sleep':
	 fun {$ N} {Delay N} unit end
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
      'Transient.ByNeed': {NewUniqueName 'Transient.ByNeed'}
      'Transient.Fulfill': {NewUniqueName 'Transient.Fulfill'}
      'Transient.Future': {NewUniqueName 'Transient.Future'}
      'Transient.Promise': {NewUniqueName 'Transient.Promise'}
      'Transient.await':
	 fun {$ X} {Wait X} X end
      'Transient.byNeed':
	 fun {$ P} {ByNeed fun {$} {P unit} end} end
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
	    if {IsFree P} then !!P
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
	 fun {$ Xs} {List.toTuple vector Xs} end
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
      'Word.mod': BootWord.'mod'
      'Word.orb': BootWord.'orb'
      'Word.xorb': BootWord.'xorb'
      'Word.andb': BootWord.'andb'
      'Word.notb': BootWord.notb
      'Word.<<': BootWord.'<<'
      'Word.>>': BootWord.'>>'
      'Word.~>>': BootWord.'~>>'
      'Word.toString':
	 fun {$ X} {ByteString.make {ToHex {BootWord.toInt X}}} end)

   Env = env('false': false
	     'true': true
	     'nil': 'nil'
	     'cons': '|'
	     'ref': NewCell
	     'Match': {NewUniqueName 'General.Match'}
	     'Bind': {NewUniqueName 'General.Bind'})
end
