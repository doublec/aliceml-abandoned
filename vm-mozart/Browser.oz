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
   Browser(browse)
export
   '$Browser': BrowserModule
define
   BrowserModule = 'Browser'('browse': fun {$ X} {Browser.browse X} unit end)
end
