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
   Open(file)
   System(show)
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

   fun {TextIOInputAll F}
      case {F getS($)} of false then ""
      [] S then S#'\n'#{TextIOInputAll F}
      end
   end

   BuiltinTable =
   builtinTable(
      '~': Number.'~'   %--** overloaded for word
      '+': fun {$ X1#X2} X1 + X2 end   %--** overloaded for word
      '-': fun {$ X1#X2} X1 - X2 end   %--** overloaded for word
      '*': fun {$ X1#X2} X1 * X2 end   %--** overloaded for word
      'div': fun {$ X1#X2} X1 div X2 end   %--** overloaded for word
      'mod': fun {$ X1#X2} X1 mod X2 end   %--** overloaded for word
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
      '<>': fun {$ X1 X2} X1 \= X2 end
      'Char.ord': fun {$ C} C end
      'Char.chr': fun {$ C} C end
      'Char.isDigit': Char.isDigit
      'Char.isHexDigit': Char.isXDigit
      'Char.isSpace': Char.isSpace
      'Char.toCString':
	 fun {$ C} {ByteString.make &"|{ToCString C "\""}} end
      'Int.compare\'':
	 fun {$ I#J}
	    if I == J then 0
	    elseif I < J then ~1
	    else 1
	    end
	 end
      'Int.toString': Int.toString
      'String.^':
	 fun {$ S1#S2} {ByteString.append S1 S2} end
      'String.toCString':
	 fun {$ X}
	    {ByteString.make &"|{FoldR {ByteString.toString X}
				 ToCString "\""}}
	 end
      'String.str':
	 fun {$ C} {ByteString.make [C]} end
      'String.size':
	 fun {$ S} {ByteString.length S} end
      'String.sub':
	 fun {$ S#I} {ByteString.get S I} end
      'String.substring':
	 fun {$ S#I#J} {ByteString.slice S I I + J} end
      'String.compare\'':
	 fun {$ S#T}
	    if {StringLess S T} then ~1
	    elseif {StringLess T S} then 1
	    else 0
	    end
	 end
      'Array.array':
	 fun {$ N#Init} {Array.new 0 N - 1 Init} end
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
	 fun {$ A#I} {Array.get A I} end
      'Array.update':
	 fun {$ A#I#X} {Array.put A I X} '#' end
      'Vector.fromList':
	 fun {$ Xs} {List.toTuple vector Xs} end
      'Vector.sub':
	 fun {$ V I} V.(I + 1) end
      'TextIO.stdIn':
	 {New Open.file init(name: stdin flags: [read])}
      'TextIO.openIn':
	 fun {$ S}
	    {New Open.file init(name: S flags: [read])}
	 end
      'TextIO.inputAll': TextIOInputAll
      'TextIO.closeIn':
	 fun {$ F} {F close()} '#' end
      'TextIO.stdOut':
	 {New Open.file init(name: stdout flags: [write])}
      'TextIO.openOut':
	 fun {$ S}
	    {New Open.file init(name: S flags: [write create truncate])}
	 end
      'TextIO.output':
	 fun {$ F#S} {F write(vs: S)} '#' end
      'TextIO.output1':
	 fun {$ F#C} {F write(vs: [C])} '#' end
      'TextIO.closeOut':
	 fun {$ F} {F close()} '#' end
      'TextIO.print':
	 fun {$ X} {System.show X} '#' end)

   Match = {NewName}
   Bind = {NewName}

   Env = env('false': false
	     'true': true
	     'nil': nil
	     'cons': '|'
	     'ref': fun {$ X} {NewCell X} end
	     'Match': Match
	     'Bind': Bind
	     'eq': fun {$ X#Y} X == Y end
	     'assign': fun {$ X#Y} {Assign X Y} '#' end
	     'builtin': fun {$ S} BuiltinTable.{VirtualString.toAtom S} end
	     '<': fun {$ X#Y} X < Y end   %--** remove
	     '+': fun {$ X#Y} X + Y end   %--** remove
	     '*': fun {$ X#Y} X * Y end)   %--** remove
end
