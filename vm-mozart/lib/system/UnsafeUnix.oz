%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Open(pipe text)
   UnsafeIO(stdText)
export
   'UnsafeUnix$': Unix
define
   class TextPipe from Open.pipe Open.text UnsafeIO.stdText
      attr cmd
      meth init(cmd: Cmd args: Args)
	 cmd <- Cmd
	 Open.pipe, init(cmd: Cmd args: Args)
      end
      meth getName($)
	 @cmd
      end
   end

   Unix =
   'Unix'('execute':
	     fun {$ Cmd Args} P in
		P = {New TextPipe init(cmd: Cmd args: Args)}
		P#P
	     end
	  'streamsOf':
	     fun {$ Instream#Outstream}
		Instream#Outstream
	     end)
end
