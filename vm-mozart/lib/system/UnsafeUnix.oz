%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Open(pipe text)
export
   'UnsafeUnix$': Unix
define
   class TextPipe from Open.pipe Open.text end

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
