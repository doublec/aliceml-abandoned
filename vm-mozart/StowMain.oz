%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2000
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Application(getArgs exit)
   Property(put get)
   Module(manager)
   System(printError show)
   MainComponent('$Composer': Composer) at 'x-alice:/top/Composer.ozf'
define
   fun {TypeCheck T1 T2}
      case T1#T2 of sig(unit)#_ then true
      [] _#sig(unit) then true
      [] sig(S1)#sig(S2) then {Composer.'$Sig'.matches S1 S2}
      else true
      end
   end

   case {Application.getArgs plain} of Name|Rest then
      {Property.put 'ozd.args' Rest}
      {{New Module.manager init(TypeCheck)} link(url: Name)}
   [] nil then
      {System.printError
       'Usage: '#{Property.get 'application.url'}#' <name> <args> ...\n'}
      {Application.exit 2}
   end
end
