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
export
   'UnsafeSocket$': Socket
define
   IoException = {NewUniqueName 'IO.Io'}   %--** does not exit any more

   Socket = 'Socket'(server:
			fun {$ TakePort} Socket Port in
			   Socket = {New Open.socket init()}
			   if TakePort \= 0 then
			      {Socket bind(takePort: TakePort)}
			      Port = TakePort
			   else
			      {Socket bind(port: Port)}
			   end
			   {Socket listen()}
			   Socket#Port
			end
		     accept:
			fun {$ Socket} NewSocket Host Port in
			   {Socket accept(acceptClass: Open.socket
					  accepted: ?NewSocket
					  host: ?Host port: ?Port)}
			   NewSocket#{ByteString.make Host}#Port
			end
		     client:
			fun {$ Host Port} Socket in
			   Socket = {New Open.socket init()}
			   {Socket connect(host: Host port: Port)}
			   Socket
			end
		     input1:
			fun {$ Socket}
			   try Cs in
			      {Socket read(list: ?Cs size: 1)}
			      case Cs of [C] then 'SOME'(C)
			      [] nil then 'NONE'
			      end
			   catch E then
			      {Exception.raiseError
			       alice(IoException(name:
						    {ByteString.make 'socket'}
						 function:
						    {ByteString.make 'input1'}
						 cause: E))} %--** not type exn
			      unit
			   end
			end
		     inputN:
			fun {$ Socket N}
			   try Cs in
			      {Socket read(list: ?Cs size: N)}
			      {ByteString.make Cs}
			   catch E then
			      {Exception.raiseError
			       alice(IoException(name:
						    {ByteString.make 'socket'}
						 function:
						    {ByteString.make 'inputN'}
						 cause: E))} %--** not type exn
			      unit
			   end
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
