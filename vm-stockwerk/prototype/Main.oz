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
   System(showError show)
   Property(get)
   Linker(link)
   PrimitiveTable(table)
   Scheduler(object)
define
   case {Application.getArgs plain} of [ComponentName] then
      Transient = {Linker.link ComponentName}
      AwaitClosure = PrimitiveTable.table.'Future.await'
      Res
   in
      {Scheduler.object newThread(AwaitClosure arg(Transient) ?Res)}
      {Scheduler.object run()}
      {System.show Res}
      {Application.exit 1}
   else
      {System.showError
       'Usage: '#{Property.get 'application.url'}#' <component>'}
      {Application.exit 2}
   end
end
