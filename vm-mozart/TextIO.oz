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
   System(printInfo)
   Open(file text)
export
   '$TextIO': TextIO
   Print
define
   class TextFile from Open.file Open.text
   end

   fun {TextIOInputAll F}
      case {F getS($)} of false then ""
      [] S then S#'\n'#{TextIOInputAll F}
      end
   end

   fun {Print X}
      {System.printInfo X} '#'
   end

   TextIO =
   'TextIO'(
      'stdIn':
	 {New TextFile init(name: stdin flags: [read])}
      'openIn':
	 fun {$ S}
	    {New TextFile init(name: S flags: [read])}
	 end
      'inputAll':
	 fun {$ F} {ByteString.make {TextIOInputAll F}} end
      'closeIn':
	 fun {$ F} {F close()} '#' end
      'stdOut':
	 {New TextFile init(name: stdout flags: [write])}
      'stdErr':
	 {New TextFile init(name: stderr flags: [write])}
      'openOut':
	 fun {$ S}
	    {New TextFile init(name: S flags: [write create truncate])}
	 end
      'output':
	 fun {$ F#S} {F write(vs: S)} '#' end
      'output1':
	 fun {$ F#C} {F write(vs: [C])} '#' end
      'closeOut':
	 fun {$ F} {F close()} '#' end
      'print': Print)
end
