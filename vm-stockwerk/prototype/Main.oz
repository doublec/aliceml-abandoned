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
   Application(getArgs exit)
   System(showError)
   Property(get)
   Linker(link)
define
   case {Application.getArgs plain} of [ComponentName] then
      _ = {Linker.link ComponentName}
      {Application.exit 1}
   else
      {System.showError
       'Usage: '#{Property.get 'application.url'}#' <component>'}
      {Application.exit 2}
   end
end
