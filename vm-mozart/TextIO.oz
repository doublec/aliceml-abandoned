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
   Open(file)
export
   '$TextIO': TextIO
   Print
define
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
	 {New Open.file init(name: stdin flags: [read])}
      'openIn':
	 fun {$ S}
	    {New Open.file init(name: S flags: [read])}
	 end
      'inputAll':
	 fun {$ F} {ByteString.make {TextIOInputAll F}} end
      'closeIn':
	 fun {$ F} {F close()} '#' end
      'stdOut':
	 {New Open.file init(name: stdout flags: [write])}
      'stdErr':
	 {New Open.file init(name: stderr flags: [write])}
      'openOut':
	 fun {$ S}
	    {New Open.file init(name: S flags: [write create truncate])}
	 end
      'output':
	 fun {$ F#S} {F write(vs: S)} '#' end
      'output1':
	 fun {$ F#C} {F write(vs: [C])} '#' end
      'closeOut':
	 fun {$ F} {F close()} '#' end
      'print': Print)
end
