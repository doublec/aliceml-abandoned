%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Application(getArgs exit)
   Property(put get)
   Module(manager)
   System(printError)
   Resolve(expand trace)
   URL(toVirtualString)
   UrlComponent('Url$': Url) at 'lib/utility/Url'
   ComposerComponent('Composer$': Composer) at 'stoc/top/Composer'
define
   Spec = record(mode: start
		 trustlocalcomponents(rightmost type: bool default: true))

   proc {Usage N}
      {System.printError
       'Usage: '#{Property.get 'application.url'}#
       ' [--trustlocalcomponents] <name> <args> ...\n'}
      {Application.exit N}
   end

   fun {Expand Url}
      {URL.toVirtualString {Resolve.expand Url}}
   end

   proc {Trace Title Msg}
      if {Resolve.trace.get} then
	 {System.printError '['#Title#'] '#Msg#'\n'}
      end
   end

   try
      Args = {Application.getArgs Spec}

      fun {CheckExpImp T1 T2}
	 case T1#T2 of _#intersection(T2a T2b) then
	    if {CheckExpImp T1 T2a} == ok then {CheckExpImp T1 T2b}
	    else no('signature mismatch')
	    end
	 [] sig(unit)#_ then ok
	 [] _#sig(unit) then ok
	 [] sig(S1)#sig(S2) then
	    if {Composer.'Sig$'.matches S1 S2} then ok
	    else no('signature mismatch')
	    end
	 else ok
	 end
      end

      fun {CheckExpImpExtended T1 T2 Info}
	 ExpUrl = {Url.fromString {ByteString.make {Expand Info.url}}}
	 IsLocal = case {Url.getScheme ExpUrl}
		   of 'NONE' then true
		   [] 'SOME'(Scheme) then
		      case {VirtualString.toAtom Scheme} of 'x-oz' then true
		      [] 'x-alice' then true
		      [] 'file' then {Url.getAuthority ExpUrl} == 'NONE'
		      else false
		      end
		   end
      in
	 if IsLocal andthen Args.trustlocalcomponents then
	    {Trace 'composer' 'trusting local URL '#Info.url}
	    ok
	 else Res in
	    {Trace 'composer' 'type-checking URL '#Info.url}
	    Res = {CheckExpImp T1 T2}
	    case Res of ok then
	       {Trace 'composer' '...type-checking succeeded'}
	    [] no(VS) then
	       {Trace 'composer' '...type-checking failed: '#VS}
	    end
	    Res
	 end
      end

      fun {CheckImpImp T1 T2}
	 ok(intersection(T1 T2))
      end
   in
      case Args.1 of Name|Rest then ModuleManager in
	 {Property.put 'ozd.args' Rest}
	 {Property.put 'ozl.checkExpImp' CheckExpImpExtended}
	 {Property.put 'ozl.checkImpImp' CheckImpImp}
	 ModuleManager = {New Module.manager init(CheckExpImpExtended)}
	 {Property.put 'alice.modulemanager' ModuleManager}
	 {Wait {ModuleManager link(url: Name $)}}
      [] nil then
	 {Usage 2}
      end
   catch error(ap(usage VS) ...) then
      {System.printError 'Usage error: '#VS#'\n'}
      {Usage 2}
   end
end
