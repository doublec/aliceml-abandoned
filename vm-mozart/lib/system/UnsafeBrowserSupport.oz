%%%
%%% Author:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
export
   'UnsafeBrowserSupport$': UnsafeBrowserSupport
define
   UnsafeBrowserSupport =
   'UnsafeBrowserSupport'('waitRequest':
			     fun {$ X}
				{Value.waitQuiet X} X
			     end)
end
