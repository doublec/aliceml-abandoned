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
   Resolve(trace)
   Error(registerFormatter)
   ComposerComponent('Composer$': Composer) at 'stoc/top/Composer'
define
   Spec = record(mode: start
		 typecheck(rightmost type: bool default: true))

   proc {Usage N}
      {System.printError
       'Usage: '#{Property.get 'application.url'}#' <name> <args> ...\n'}
      {Application.exit N}
   end

   proc {Trace Title Msg}
      if {Resolve.trace.get} then
	 {System.printError '['#Title#'] '#Msg#'\n'}
      end
   end

   fun {CheckExpImp T1 T2}
      case T1#T2 of _#intersection(T2a T2b) then
	 if {CheckExpImp T1 T2a} == ok then {CheckExpImp T1 T2b}
	 else no('signature mismatch')
	 end
      [] sig(_#unit)#_ then ok
      [] _#sig(_#unit) then ok
      [] sig(S1)#sig(S2) then
	 if {Composer.'Sig$'.matches S1 S2} then ok
	 else no('signature mismatch')
	 end
      else ok
      end
   end

   fun {CheckExpImpExtended T1 T2 Info} Res in
      {Trace 'composer' 'type-checking '#Info.url}
      Res = {CheckExpImp T1 T2}
      case Res of ok then
	 {Trace 'composer' '...type-checking succeeded'}
      [] no(VS) then
	 {Trace 'composer' '...type-checking failed: '#VS}
      end
      Res
   end

   fun {FindIdentity T N}
      case T of sig(N0#_) then N == N0
      [] intersection(T1 T2) then
	 {FindIdentity T1 N} orelse {FindIdentity T2 N}
      else false
      end
   end

   fun {CheckImpImp T1 T2}
      case T1 of sig(N#_) andthen {FindIdentity T2 N} then ok(T2)
      else ok(intersection(T1 T2))
      end
   end

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

   try
      Args = {Application.getArgs Spec}
   in
      case Args.1 of Name|Rest then
	 ModuleManager =
	 if Args.typecheck then
	    {Property.put 'ozl.checkExpImp' CheckExpImpExtended}
	    {Property.put 'ozl.checkImpImp' CheckImpImp}
	    {New Module.manager init(CheckExpImpExtended)}
	 else
	    {New Module.manager init()}
	 end
      in
	 {Property.put 'alice.modulemanager' ModuleManager}
	 {Property.put 'ozd.args' Rest}
	 {Property.put 'errors.depth' 20}
	 {Property.put 'errors.width' 10}
	 {Wait {ModuleManager link(url: Name $)}}
      [] nil then
	 {Usage 2}
      end
   catch error(ap(usage VS) ...) then
      {System.printError 'Usage error: '#VS#'\n'}
      {Usage 2}
   end
end
