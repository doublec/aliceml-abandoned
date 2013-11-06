%%%
%%% Author:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2004
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
export
   'UnsafeSignal$' : UnsafeSignal
define
   UnsafeSignal =
   'Signal'('register' : fun {$ Signal Closure} unit end)
end
