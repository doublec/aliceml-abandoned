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
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   System(show)
   Application(exit)
export
   BuiltinTable
   Env
define
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

   BuiltinTable =
   builtinTable(
      'show': fun {$ X} {System.show X} unit end
      '=': fun {$ X Y} X == Y end
      ':=': fun {$ X Y} {Assign X Y} unit end
      '~': Number.'~'   %--** overloaded for word
      '+': fun {$ X1 X2} X1 + X2 end   %--** overloaded for word
      '-': fun {$ X1 X2} X1 - X2 end   %--** overloaded for word
      '*': fun {$ X1 X2} X1 * X2 end   %--** overloaded for word
      'div':
	 fun {$ X1 X2}   %--** overloaded for word
	    try
	       X1 div X2
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Div'} unit
	    end
	 end
      'mod':
	 fun {$ X1 X2}   %--** overloaded for word
	    X1 mod X2
	 end
      '<':
	 fun {$ X1 X2}
	    if {ByteString.is X1} then {StringLess X1 X2} else X1 < X2 end
	 end
      '>':
	 fun {$ X1 X2}
	    if {ByteString.is X1} then {StringLess X2 X1} else X1 > X2 end
	 end
      '<=':
	 fun {$ X1 X2}
	    if {ByteString.is X1} then {Not {StringLess X2 X1}}
	    else X1 =< X2
	    end
	 end
      '>=':
	 fun {$ X1 X2}
	    if {ByteString.is X1} then {Not {StringLess X1 X2}}
	    else X1 >= X2
	    end
	 end
      '<>': fun {$ X1 X2} X1 \= X2 end
      'Application.exit':   %--** does not belong here
	 proc {$ I _} {Application.exit I} end
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
      'Int.compare':
	 fun {$ I J}
	    if I == J then 'EQUAL'
	    elseif I < J then 'LESS'
	    else 'GREATER'
	    end
	 end
      'Int.toString':
	 fun {$ I} {ByteString.make {Int.toString I}} end
      'List.Empty': {NewUniqueName 'List.Empty'}
      'Option.Option': {NewUniqueName 'Option.Option'}
      'String.^':
	 fun {$ S1 S2} {ByteString.append S1 S2} end
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
      'Thread.getCurrent':
	 fun {$ unit} {Thread.this} end
      'Thread.getState':
	 fun {$ T}
	    case {Thread.state T} of runnable then 'RUNNABLE'
	    [] blocked then 'BLOCKED'
	    [] terminated then 'TERMINATED'
	    end
	 end
      'Thread.injectException':
	 fun {$ T E} {Thread.injectException T E} unit end
      'Thread.isSuspended':
	 fun {$ T} {Thread.isSuspended T} end
      'Thread.preempt':
	 fun {$ T} {Thread.preempt T} unit end
      'Thread.resume':
	 fun {$ T} {Thread.resume T} unit end
      'Thread.sleep':
	 fun {$ N} {Delay N} unit end
      'Thread.spawn':
	 fun {$ P} thread _ = {P unit} end unit end
      'Thread.suspend':
	 fun {$ T} {Thread.suspend T} unit end
      'Transient.await':
	 fun {$ X} {Wait X} X end
      'Transient.byNeed':
	 fun {$ P} {ByNeed fun {$} {P unit} end} end
      'Transient.fulfill':
	 fun {$ P X} P = X unit end
      'Transient.future':
	 fun {$ P} !!P end
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
	 end)

   Env = env('false': false
	     'true': true
	     'nil': 'nil'
	     'cons': '|'
	     'ref': NewCell
	     'Match': {NewUniqueName 'General.Match'}
	     'Bind': {NewUniqueName 'General.Bind'})
end
