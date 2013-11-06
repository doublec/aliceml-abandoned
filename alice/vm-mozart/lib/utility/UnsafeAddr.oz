%%%
%%% Author:
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Andreas Rossberg, 2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
    M at 'x-oz://boot/Browser'
export
    'addr' : Addr
define
    Addr = M.addr
end
