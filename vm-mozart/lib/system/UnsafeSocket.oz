%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Open(socket)
   System(show)
export
   'UnsafeSocket$': Socket
define
   Socket = 'Socket'(server:
			fun {$ PortOpt F} Socket Port in
			   Socket = {New Open.socket init()}
			   case PortOpt of 'NONE' then
			      {Socket bind(port: ?Port)}
			   [] 'SOME'(TakePort) then
			      {Socket bind(takePort: TakePort port: ?Port)}
			   end
			   {Socket listen()}
			   thread Accept in
			      proc {Accept} NewSocket Host Port in
				 {System.show waiting}
				 {Socket accept(acceptClass: Open.socket
						accepted: ?NewSocket
						host: ?Host port: ?Port)}
				 {System.show found}
				 _ = {F NewSocket Host Port}
				 {Accept}
			      end
			      {Accept}
			   end
			   Socket#Port
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
		     output:
			fun {$ Socket S}
			   {Socket write(vs: S)}
			   unit
			end
		     output1:
			fun {$ Socket C}
			   {Socket write(vs: [C])}
			   unit
			end
		     close:
			fun {$ Socket}
			   {Socket close()}
			   unit
			end)
end
