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

   BuiltinTable =
   builtinTable(
      'System.show': fun {$ X} {System.show X} '#' end
      '~': Number.'~'   %--** overloaded for word
      '+': fun {$ X1#X2} X1 + X2 end   %--** overloaded for word
      '-': fun {$ X1#X2} X1 - X2 end   %--** overloaded for word
      '*': fun {$ X1#X2} X1 * X2 end   %--** overloaded for word
      'div': fun {$ X1#X2} X1 div X2 end   %--** overloaded for word
      'mod': fun {$ X1#X2} X1 mod X2 end   %--** overloaded for word
      '<': fun {$ X1#X2}
	      if {ByteString.is X1} then {StringLess X1 X2} else X1 < X2 end
	   end
      '>': fun {$ X1#X2}
	      if {ByteString.is X1} then {StringLess X2 X1} else X1 > X2 end
	   end
      '<=': fun {$ X1#X2}
	       if {ByteString.is X1} then {Not {StringLess X2 X1}}
	       else X1 =< X2
	       end
	    end
      '>=': fun {$ X1#X2}
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
      'Char.toCString': fun {$ C} {ByteString.make &"|{ToCString C "\""}} end
      'Int.compare\'': fun {$ I#J}
			  if I == J then 0
			  elseif I < J then ~1
			  else 1
			  end
		       end
      'Int.toString': Int.toString
      'String.^': fun {$ S1#S2} {ByteString.append S1 S2} end
      'String.toCString': fun {$ X}
			     {ByteString.make &"|{FoldR {ByteString.toString X}
						  ToCString "\""}}
			  end
      'String.str': fun {$ C} {ByteString.make [C]} end
      'String.size': fun {$ S} {ByteString.length S} end
      'String.sub': fun {$ S#I} {ByteString.get S I} end
      'String.substring': fun {$ S#I#J} {ByteString.slice S I I + J} end
      'String.compare\'': fun {$ S#T}
			     if {StringLess S T} then ~1
			     elseif {StringLess T S} then 1
			     else 0
			     end
			  end
      )

   Match = {NewName}
   Bind = {NewName}

   Env = env('false': v#false
	     'true': v#true
	     'nil': v#nil
	     'cons': c#fun {$ X#Y} X|Y end#'|'
	     'ref': v#fun {$ X} {NewCell X} end
	     'Match': v#Match
	     'Bind': v#Bind
	     'eq': v#fun {$ X#Y} X == Y end
	     'assign': v#fun {$ X#Y} {Assign X Y} '#' end
	     'builtin': v#fun {$ S} BuiltinTable.{VirtualString.toAtom S} end
	     '<': v#fun {$ X#Y} X < Y end   %--** remove
	     '+': v#fun {$ X#Y} X + Y end   %--** remove
	     '*': v#fun {$ X#Y} X * Y end)   %--** remove
end
