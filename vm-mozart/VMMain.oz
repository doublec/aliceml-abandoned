%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Application(getArgs exit)
   Property(get put)
   System(printError onToplevel)
   Error(registerFormatter printException)
   BootComponent('boot': Boot) at 'lib/system/Boot'
define
   {Error.registerFormatter alice
    fun {$ E} T in
       T = 'Alice exception'
       case E of alice(E Coord) then
	  error(kind: T
		items: [hint(l: 'Exception' m: oz(E))
			hint(l: 'Raised at' m: Coord)])
       [] alice(failed F I J) then
	  error(kind: T
		msg: 'Evaluated failed expression'
		items: [hint(l: 'At' m: pos(F I J))])
       [] alice(InnerE ...) then
	  error(kind: T
		items: (hint(l: 'Exception' m: oz(InnerE))|
			{List.mapInd {Record.toList E}.2
			 fun {$ I X} hint(l: 'Debug '#I m: oz(X)) end}))
       else
	  error(kind: T
		items: [line(oz(E))])
       end
    end}

   {Property.put 'alice.atExnActions'
    {NewCell [fun {$ E}
		 {Error.printException E}
		 if {System.onToplevel} then
		    {Application.exit 1}
		 end
		 unit
	      end]}}

   {Property.put 'errors.handler'
    proc {$ E}
       case E of system(kernel(terminate) ...) then
	  skip
       [] error(alice(Exn ...) ...) then
	  {ForAll {Access {Property.get 'alice.atExnActions'}}
	   proc {$ P}
	      try {P Exn _} catch _ then skip end
	   end}
       else
	  {Error.printException E}
	  if {System.onToplevel} then
	     {Application.exit 1}
	  end
       end
    end}

   case {Application.getArgs plain} of Name|Rest then
      {Property.put 'alice.rootUrl' Name}
      {Property.put 'ozd.args' Rest}
      {Property.put 'errors.depth' 20}
      {Property.put 'errors.width' 10}
      {Boot {ByteString.make Name} _}
   [] nil then
      {System.printError 'Usage: alicerun <name> <args> ...\n'}
      {Application.exit 2}
   end
end
