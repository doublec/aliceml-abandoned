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
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   Open(socket text)
   Property(get)
export
   module: SocketComponent
define
   SocketComponent = tuple(Socket)

   NONE = 0
   SOME = 1

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

   I_client    = 1
   I_close     = 2
   I_flushOut  = 3
   I_input1    = 4
   I_inputLine = 5
   I_inputN    = 6
   I_output    = 7
   I_output1   = 8
   I_server    = 9

   Socket =
   tuple(I_server:
	    fun {$ PortOpt F} Socket Port in
	       Socket = {New TextSocket init()}
	       case PortOpt of !NONE then
		  {Socket bind(port: ?Port)}
	       [] tag(!SOME TakePort) then   %--** request TakePort
		  {Socket bind(takePort: TakePort port: ?Port)}
	       end
	       {Socket listen()}
	       thread Accept in   %--** use a Scheduler thread?
		  proc {Accept} NewSocket Host Port in
		     {Socket accept(acceptClass: TextSocket
				    accepted: ?NewSocket
				    host: ?Host port: ?Port)}
		     %--** apply using pushCall
		     _ = {F NewSocket Host Port}
		     {Accept}
		  end
		  {Accept}
	       end
	       tuple(Socket Port)
	    end#rr_v
	 I_client:
	    fun {$ Host Port} Socket in
	       Socket = {New TextSocket init()}
	       {Socket connect(host: Host port: Port)}
	       Socket
	    end#rr_v
	 I_input1:
	    fun {$ Socket} Cs in
	       {Socket read(list: ?Cs size: 1)}
	       case Cs of [C] then tag(SOME C)
	       [] nil then NONE
	       end
	    end#r_v
	 I_inputN:
	    fun {$ Socket N}
	       {ByteString.make {Socket read(list: $ size: N)}}
	    end#rr_v
	 I_inputLine:
	    fun {$ Socket}
	       case {Socket getS($)}
	       of false then {ByteString.make ""}
	       elseof S then {ByteString.make {ConvertLine S}#'\n'}
	       end
	    end#r_v
	 I_output:
	    fun {$ Socket S TaskStack}
	       try
		  continue(arg({Socket write(vs: S)}) TaskStack.2)
	       catch E then
		  exception(nil con(IoException
				    name: {ByteString.make 'socket'}
				    function: {ByteString.make 'output'}
				    cause: E)   %--** not type exn
			    TaskStack.2)
	       end
	    end#rr_t
	 I_output1:
	    fun {$ Socket C TaskStack}
	       try
		  {Socket write(vs: [C])}
		  continue(args() TaskStack.2)
	       catch E then
		  exception(nil con(IoException
				    name: {ByteString.make 'socket'}
				    function: {ByteString.make 'output1'}
				    cause: E)
			    TaskStack.2)   %--** not type exn
	       end
	    end#rr_t
	 I_flushOut:
	    fun {$ Socket}
	       {Socket flush()}
	       tuple()
	    end#r_v
	 I_close:
	    fun {$ Socket}
	       {Socket close()}
	       tuple()
	    end#r_v)
end
