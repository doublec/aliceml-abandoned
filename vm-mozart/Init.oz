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
   System(show)
   Word at 'Word.so{native}'
define
   {Wait Word}
   case {Application.getCmdArgs plain} of Name|Rest then M in
      {System.show Rest}
      {Property.put 'ozd.args' Rest}
      [M] = {Module.link [Name]}
      {Wait M}
   end
end
