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
   OS(getEnv)
   Property(get put)
   URL(make resolve toString)
   Linker(link)
   PrimitiveTable(values)
   Scheduler(object)
define
   Spec = record(booturl(single type: string default: 'lib/system/Boot'))
   Args = {Application.getArgs Spec}

   case {OS.getEnv 'STOCKHOME'} of false then U in
      U = {URL.make {Property.get 'application.url'}}
      {Property.put 'alice.home' {URL.toString {URL.resolve U ''}}}
   elseof S then
      {Property.put 'alice.home' S#'/'}
   end

   case Args.1 of RootUrl|Rest then
      {Property.put 'stockwerk.args' Rest}
      case {Linker.link Args.booturl} of tuple(Closure) then
	 {Scheduler.object newThread(closure: Closure
				     args: arg({ByteString.make RootUrl}))}
	 {Scheduler.object run()}
	 {Application.exit 1}
      end
   else
      {System.showError
       'Usage: '#{Property.get 'application.url'}#' <component>'}
      {Application.exit 2}
   end
end
