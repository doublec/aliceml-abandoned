%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Open(pipe text)
export
   module: UnixComponent
define
   UnixComponent = tuple(Unix)

   class TextPipe from Open.pipe Open.text end

   I_execute   = 1
   I_streamsOf = 2

   Unix =
   tuple(I_execute:
	     fun {$ Cmd Args}
		P = {New TextPipe init(cmd: Cmd args: Args)} in P#P
	     end#rr_v
	 I_streamsOf:
	     fun {$ Instream#Outstream} tuple(Instream Outstream) end#rr_v)
end
