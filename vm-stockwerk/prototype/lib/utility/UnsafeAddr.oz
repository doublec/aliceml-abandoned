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
   BrowserSuppord(addr: Addr) at 'x-oz://boot/Browser'
export
   module: UnsafeAddrComponent
define
   UnsafeAddrComponent = tuple(UnsafeAddr)

   I_addr = 1

   UnsafeAddr = tuple(I_addr: Addr#i_v)
end
