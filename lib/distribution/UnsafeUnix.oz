%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Open(pipe text)
export
   'UnsafeUnix$': UnsafeUnix
define
   class TextPipe from Open.pipe Open.text end

   UnsafeUnix =
   'UnsafeUnix'(
      '$proc': {Value.byNeedFail rttNotImplemented}
      'execute':
	 fun {$ Cmd Args} P in
	    P = {New TextPipe init(cmd: Cmd args: Args)}
	    P#P
	 end
      'streamsOf':fun {$ X} X end)
end
