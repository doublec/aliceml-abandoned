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
   System(show printInfo)
export
   BuiltinTable
   Env
define
   fun {StringLess S1 S2}
      {VirtualString.toAtom S1} < {VirtualString.toAtom S2}   %--* inefficient
   end

   CStringTab =
   cStringTab(&\\: '\\\\'
	      &": '\\"'
	      &?: '\\?'
	      &': '\\\''
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

   fun {ToCString C Rest}
      case {CondSelect CStringTab C unit} of unit then
	 if {Char.isPrint C} then C|Rest
	 else &\\|{ToOct C div 64}|{ToOct C div 8}|{ToOct C}|Rest
	 end
      elseof S then {Append S Rest}
      end
   end

   fun {ImportList Xs}
      case Xs of '::'(C#Cr) then C|{ImportList Cr}
      [] nil then nil
      end
   end

   BuiltinTable =
   builtinTable(
      'show': fun {$ X} {System.show X} '#' end
      '=': fun {$ X#Y} X == Y end
      ':=': fun {$ X#Y} {Assign X Y} '#' end
      '~': Number.'~'   %--** overloaded for word
      '+': fun {$ X1#X2} X1 + X2 end   %--** overloaded for word
      '-': fun {$ X1#X2} X1 - X2 end   %--** overloaded for word
      '*': fun {$ X1#X2} X1 * X2 end   %--** overloaded for word
      'div':
	 fun {$ X1#X2}   %--** overloaded for word
	    try
	       X1 div X2
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Div'} '#'
	    end
	 end
      'mod':
	 fun {$ X1#X2}   %--** overloaded for word
	    X1 mod X2
	 end
      '<':
	 fun {$ X1#X2}
	    if {ByteString.is X1} then {StringLess X1 X2} else X1 < X2 end
	 end
      '>':
	 fun {$ X1#X2}
	    if {ByteString.is X1} then {StringLess X2 X1} else X1 > X2 end
	 end
      '<=':
	 fun {$ X1#X2}
	    if {ByteString.is X1} then {Not {StringLess X2 X1}}
	    else X1 =< X2
	    end
	 end
      '>=':
	 fun {$ X1#X2}
	    if {ByteString.is X1} then {Not {StringLess X1 X2}}
	    else X1 >= X2
	    end
	 end
      '<>': fun {$ X1#X2} X1 \= X2 end
      'Char.ord': fun {$ C} C end
      'Char.chr':
	 fun {$ C}
	    if {Char.is C} then C
	    else {Exception.raiseError BuiltinTable.'General.Chr'} '#'
	    end
	 end
      'Char.isDigit': Char.isDigit
      'Char.isHexDigit': Char.isXDigit
      'Char.isSpace': Char.isSpace
      'Char.toCString':
	 fun {$ C} {ByteString.make &"|{ToCString C "\""}} end
      'General.Chr': {NewUniqueName 'General.Chr'}
      'General.Div': {NewUniqueName 'General.Div'}
      'General.Domain': {NewUniqueName 'General.Domain'}
      'General.Fail': {NewUniqueName 'General.Fail'}
      'General.Overflow': {NewUniqueName 'General.Overflow'}
      'General.Size': {NewUniqueName 'General.Size'}
      'General.Span': {NewUniqueName 'General.Span'}
      'General.Subscript': {NewUniqueName 'General.Subscript'}
      'Int.compare\'':
	 fun {$ I#J}
	    if I == J then 0
	    elseif I < J then ~1
	    else 1
	    end
	 end
      'Int.toString':
	 fun {$ I} {ByteString.make {Int.toString I}} end
      'List.Empty': {NewUniqueName 'List.Empty'}
      'Option.Option': {NewUniqueName 'Option.Option'}
      'String.^':
	 fun {$ S1#S2} {ByteString.append S1 S2} end
      'String.toCString':
	 fun {$ X}
	    {ByteString.make &"|{FoldR {ByteString.toString X} ToCString "\""}}
	 end
      'String.str':
	 fun {$ C} {ByteString.make [C]} end
      'String.size':
	 fun {$ S} {ByteString.length S} end
      'String.sub':
	 fun {$ S#I}
	    try
	       {ByteString.get S I}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'} '#'
	    end
	 end
      'String.substring':
	 fun {$ S#I#J}
	    try
	       {ByteString.slice S I I + J}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'} '#'
	    end
	 end
      'String.compare\'':
	 fun {$ S#T}
	    if {StringLess S T} then ~1
	    elseif {StringLess T S} then 1
	    else 0
	    end
	 end
      'String.explode':
	 fun {$ S}
	    {List.foldR {ByteString.toString S}
	     fun {$ C Cr}
		'::'('#'(C Cr))
	     end nil}
	 end
      'Array.array':
	 fun {$ N#Init} {Array.new 0 N - 1 Init} end
      'Array.fromList':
	 fun {$ Xs} Xs2 N A in
	    Xs2 = {ImportList Xs}
	    N = {Length Xs2}
	    A = {Array.new 0 N - 1 unit}
	    {List.forAllInd Xs2 proc {$ I X} {Array.put A I - 1 X} end}
	    A
	 end
      'Array.length':
	 fun {$ A} {Array.high A} + 1 end
      'Array.sub':
	 fun {$ A#I}
	    try
	       {Array.get A I}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'} '#'
	    end
	 end
      'Array.update':
	 fun {$ A#I#X}
	    try
	       {Array.put A I X}
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'}
	    end
	    '#'
	 end
      'Vector.fromList':
	 fun {$ Xs} {List.toTuple vector {ImportList Xs}} end
      'Vector.sub':
	 fun {$ V#I}
	    try
	       V.(I + 1)
	    catch _ then
	       {Exception.raiseError BuiltinTable.'General.Subscript'} '#'
	    end
	 end)

   Env = env('false': false
	     'true': true
	     'nil': nil
	     'cons': '::'
	     'ref': NewCell
	     'Match': {NewUniqueName 'General.Match'}
	     'Bind': {NewUniqueName 'General.Bind'})
end
