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
   Property(get put)
   Linker(link)
   PrimitiveTable(values)
   Scheduler(object)
define
   Args = {Application.getArgs record()}

   local
      U = {URL.make {Property.get 'application.url'}}
   in
      {Property.put 'alice.home' {URL.toString {URL.resolve U ''}}}
   end

   case Args.1 of ComponentName|Rest then
      Transient = {Linker.link ComponentName}
      AwaitClosure = PrimitiveTable.values.'Future.await'
      Res
   in
      {Property.put 'stockwerk.args' Rest}
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
