%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
%   Pickle at 'x-oz://boot/Pickle'
   Connection
   Property
   OS
export
   'UnsafeRemoteCore$' : UnsafeRemoteCore
define
   %% Interface Functions
   fun {TakeFun Ticket}
%      {Pickle.unpack {Connection.take Ticket}}
      {Connection.take Ticket}
   end
   fun {OfferFun Package}
%      {ByteString.make {Connection.offerUnlimited {Pickle.packWithCells Package}}}
      {ByteString.make {Connection.offerUnlimited Package}}
   end
   fun {ProxyFun Proc}
      Error = {NewName}
      MsgPrt MsgStream
   in
      MsgPrt = {Port.new MsgStream}
      thread
	 {ForAll MsgStream
	  proc {$ Arg#ResPrt}
	     {Port.send ResPrt try {Proc Arg} catch E then Error(E) end}
	  end}
      end
      fun {$ Arg}
	 ResPrt ResStream
      in
	 ResPrt = {Port.new ResStream}
	 {Port.send MsgPrt Arg#ResPrt}
	 case ResStream
	 of Error(E)|_ then raise E end
	 [] Result|_   then Result
	 end
      end
   end
   fun {TimeFun _}
      {Property.get 'time'}.total
   end
   fun {NowFun _}
      {OS.time}
   end
   
   %% Create Interface
   UnsafeRemoteCore = 'UnsafeRemoteCore'(take : TakeFun
					 offer : OfferFun
					 proxy : ProxyFun
					 time : TimeFun
					 now : NowFun)
end
