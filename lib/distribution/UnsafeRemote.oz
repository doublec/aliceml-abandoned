%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   Alice(rpc) at 'x-oz://boot/Alice'
   Pickle(packWithCells unpack) at 'x-oz://boot/Pickle'
   Property(put get)
   Connection(offerUnlimited take)
export
   'UnsafeRemote$' : Remote
define
   SitedException = {NewUniqueName 'Pickle.Sited'}
   TicketException = {NewUniqueName 'Remote.Ticket'}

   {Property.put 'alice.rpc'
    fun {$ Ticket X} Res in
       try
	  {Port.send {Connection.take Ticket} {Pickle.packWithCells X}#Res}
       catch error(dp(generic 'pickle:nogoods' ...) ...) then
	  {Exception.raiseError alice(SitedException)}
       [] error(connection(illegalTicket ...) ...) then
	  {Exception.raiseError alice(TicketException)}
       end
       case {Pickle.unpack Res} of result(X) then X
       [] exception(E) then raise E end
       end
    end}

   Remote =
   'Remote'('Ticket': TicketException
	    '\'Ticket': TicketException
	    proxy:
	       fun {$ F} Ticket Xs in
		  Ticket = {Connection.offerUnlimited {Port.new ?Xs}}
		  thread
		     {ForAll Xs
		      proc {$ X#Res}
			 try
			    Res = {Pickle.packWithCells
				   try
				      result({F {Pickle.unpack X}})
				   catch system(E ...) then
				      exception(system(E))
				   [] error(E ...) then
				      exception(error(E))
				   [] E then
				      exception(E)
				   end}
			 catch error(dp(generic 'pickle:nogoods' ...) ...) then
			    {Exception.raiseError alice(SitedException)}
			 end
		      end}
		  end
		  fun {$ X}
		     {Alice.rpc Ticket X}
		  end
	       end
	    offer:
	       fun {$ Package}
		  try
		     {ByteString.make {Connection.offerUnlimited
				       {Pickle.packWithCells Package}}}
		  catch error(dp(generic 'pickle:nogoods' ...) ...) then
		     {Exception.raiseError alice(SitedException)} unit
		  end
	       end
	    take:
	       fun {$ Ticket}
		  try
		     {Pickle.unpack {Connection.take Ticket}}
		  catch error(connection(illegalTicket ...) ...) then
		     {Exception.raiseError alice(TicketException)} unit
		  end
	       end)
end
