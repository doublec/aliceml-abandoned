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
   System(printInfo)
   Open(file text)
export
   'TextIO$': TextIO
   Print
define
   IoException = {NewUniqueName 'IO.Io'}

   class TextFile from Open.file Open.text end

   fun {TextIOInputAll F}
      case {F getS($)} of false then ""
      [] S then S#'\n'#{TextIOInputAll F}
      end
   end

   fun {Print X}
      {System.printInfo X} unit
   end

   TextIO =
   'TextIO'(
      'stdIn':
	 {New TextFile init(name: stdin flags: [read])}
      'openIn':
	 fun {$ S}
	    try
	       {New TextFile init(name: S flags: [read])}
	    catch system(E=os(os ...) ...) then
	       {Exception.raiseError
		IoException('#'(name: S
				function: {ByteString.make 'openIn'}
				cause: E))}   %--** cause not of type exn
	       unit
	    end
	 end
      'inputAll':
	 fun {$ F} {ByteString.make {TextIOInputAll F}} end
      'inputLine':
	 fun {$ F}
	    case {F getS($)} of false then {ByteString.make ""}
	    elseof S then {ByteString.make S#'\n'}
	    end
	 end
      'closeIn':
	 fun {$ F} {F close()} unit end
      'stdOut':
	 {New TextFile init(name: stdout flags: [write])}
      'stdErr':
	 {New TextFile init(name: stderr flags: [write])}
      'openOut':
	 fun {$ S}
	    {New TextFile init(name: S flags: [write create truncate])}
	 end
      'output':
	 fun {$ F S} {F write(vs: S)} unit end
      'output1':
	 fun {$ F C} {F write(vs: [C])} unit end
      'flushOut':
	 fun {$ F} {F flush()} end   %--** not supported for files?
      'closeOut':
	 fun {$ F} {F close()} unit end
      'print': Print)
end
