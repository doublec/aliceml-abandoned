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
   Application(getCmdArgs)
   Property(put)
   Module(link)
   Word at 'Word.so{native}'
define
   {Wait Word}
   case {Application.getCmdArgs plain} of Name|Rest then M in
      {Property.put 'ozd.args' Rest}
      [M] = {Module.link [Name]}
      {Wait M}
   end
end
