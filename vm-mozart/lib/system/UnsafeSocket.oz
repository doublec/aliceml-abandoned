%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   Open(socket text)
   Property(get)
   System(show)
export
   'UnsafeSocket$': Socket
define
   IoException = {NewUniqueName 'IO.Io'}

   Socket = 'Socket'(server:
			fun {$ TakePort} Socket Port in
			   Socket = {New Open.socket init()}
			   {Socket bind(takePort: TakePort port: ?Port)}
			   {Socket listen()}
			   Socket#Port
			end
		     accept:
			fun {$ Socket} NewSocket Host Port in
			   {Socket accept(acceptClass: Open.socket
					  accepted: ?NewSocket
					  host: ?Host port: ?Port)}
			   NewSocket#Host#Port
			end
		     client:
			fun {$ Host Port} Socket in
			   Socket = {New Open.socket init()}
			   {Socket connect(host: Host port: Port)}
			   Socket
			end
		     input1:
			fun {$ Socket} Cs in
			   {Socket read(list: ?Cs size: 1)}
			   case Cs of [C] then 'SOME'(C)
			   [] nil then 'NONE'
			   end
			end
		     inputN:
			fun {$ Socket N} Cs in
			   {Socket read(list: ?Cs size: N)}
			   {ByteString.make Cs}
			end
		     output1:
			fun {$ Socket C}
			   try
			      {Socket write(vs: [C])}
			   catch E then
			      {Exception.raiseError
			       alice(IoException(name:
						    {ByteString.make 'socket'}
						 function:
						    {ByteString.make 'output1'}
						 cause: E))} %--** not type exn
			   end
			   unit
			end
		     output:
			fun {$ Socket S 0}
			   try
			      {Socket write(vs: S len: $)}
			   catch E then
			      {Exception.raiseError
			       alice(IoException(name:
						    {ByteString.make 'socket'}
						 function:
						    {ByteString.make 'output'}
						 cause: E))} %--** not type exn
			      unit
			   end
			end
		     close:
			fun {$ Socket}
			   {Socket close()}
			   unit
			end)
end
