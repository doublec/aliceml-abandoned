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
   Application(getArgs exit)
   Property(put get)
   Module(link)
   System(printError)
   Word at 'Word.so{native}'
define
   {Wait Word}
   {Property.put 'errors.depth' 10}
   {Property.put 'errors.width' 10}
   case {Application.getArgs plain} of Name|Rest then M in
      {Property.put 'ozd.args' Rest}
      [M] = {Module.link [Name]}
      {Wait M}
   [] nil then
      {System.printError
       'usage: '#{Property.get 'application.url'}#' <name> <args> ...\n'}
      {Application.exit 2}
   end
end
