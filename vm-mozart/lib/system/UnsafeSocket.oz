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
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   Open(socket text)
   Property(get)
   System(show)
export
   'UnsafeSocket$': Socket
define
   IoException = {NewUniqueName 'IO.Io'}

   class TextSocket from Open.socket Open.text end

   ConvertLine ConvertAll
   case {Property.get 'platform.os'} of win32 then
      fun {ConvertLine S}
	 case S of [&\r] then nil
	 [] C|Cr then C|{ConvertLine Cr}
	 [] nil then nil
	 end
      end
      fun {ConvertAll S}
	 case S of &\r|&\n|Rest then &\n|{ConvertAll Rest}
	 [] C|Cr then C|{ConvertAll Cr}
	 [] nil then nil
	 end
      end
   else
      fun {ConvertLine S} S end
      fun {ConvertAll S} S end
   end

   Socket = 'Socket'(server:
			fun {$ PortOpt F} Socket Port in
			   Socket = {New TextSocket init()}
			   case PortOpt of 'NONE' then
			      {Socket bind(port: ?Port)}
			   [] 'SOME'(TakePort) then
			      {Socket bind(takePort: TakePort port: ?Port)}
			   end
			   {Socket listen()}
			   thread Accept in
			      proc {Accept} NewSocket Host Port in
				 {System.show waiting}
				 {Socket accept(acceptClass: TextSocket
						accepted: ?NewSocket
						host: ?Host port: ?Port)}
				 {System.show found}
				 case {Procedure.arity F} of 2 then
				    _ = {F '#'(NewSocket Host Port)}
				 [] 4 then
				    _ = {F NewSocket Host Port}
				 end
				 {Accept}
			      end
			      {Accept}
			   end
			   Socket#Port
			end
		     client:
			fun {$ Host Port} Socket in
			   Socket = {New TextSocket init()}
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
		     inputLine:
			fun {$ Socket}
			   case {Socket getS($)}
			   of false then {ByteString.make ""}
			   elseof S then {ByteString.make {ConvertLine S}#'\n'}
			   end
			end
		     output:
			fun {$ Socket S}
			   try
			      {Socket write(vs: S)}
			   catch E then
			      {Exception.raiseError
			       alice(IoException(name:
						    {ByteString.make 'socket'}
						 function:
						    {ByteString.make 'output'}
						 cause: E))} %--** not type exn
			   end
			   unit
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
		     flushOut:
			fun {$ Socket}
			   {Socket flush()}
			   unit
			end
		     close:
			fun {$ Socket}
			   {Socket close()}
			   unit
			end)
end
