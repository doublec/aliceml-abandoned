%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Open(text pipe)
   Compiler(engine interface)
export
   TranslateFile
define
   class TextPipe from Open.pipe Open.text end

   fun {StockhausenToImperative File} Pipe S in
      Pipe = {New TextPipe
	      init(cmd: 'sml-cm'
		   args: ['@SMLload=../top/stoc-frontend' File])}
      {Pipe getS(?S)}
      {Pipe close()}
      S
   end

   proc {Share I ShareDict NewStm}
      if {Dictionary.member ShareDict I} then
	 NewStm = {Dictionary.get ShareDict I}
      else
	 {Dictionary.put ShareDict I NewStm}
      end
   end

   fun {ShareStm Stm ShareDict}
      case Stm of valDec(Coord Id Exp IsToplevel) then
	 valDec(Coord Id {ShareExp Exp ShareDict} IsToplevel)
      [] recDec(Coord IdExpList IsToplevel) then
	 recDec(Coord {Map IdExpList
		       fun {$ Id#Exp} Id#{ShareExp Exp ShareDict} end}
		IsToplevel)
      [] conDec(_ _ _ _) then Stm
      [] evalStm(Coord Exp) then evalStm(Coord {ShareExp Exp ShareDict})
      [] handleStm(Coord Body1 Id Body2) then
	 handleStm(Coord {ShareBody Body1 ShareDict}
		   Id {ShareBody Body2 ShareDict})
      [] endHandleStm(Coord Body) then
	 endHandleStm(Coord {ShareBody Body ShareDict})
      [] testStm(Coord Id Test Body1 Body2) then
	 testStm(Coord Id Test
		 {ShareBody Body1 ShareDict} {ShareBody Body2 ShareDict})
      [] raiseStm(_ _) then Stm
      [] sharedStm(Coord Body I) then NewStm in
	 {Share I ShareDict NewStm}
	 NewStm = sharedStm(Coord {ShareBody Body ShareDict} I)
      [] refStm(I) then
	 {Share I ShareDict}
      [] returnStm(Coord Exp) then
	 returnStm(Coord {ShareExp Exp ShareDict})
      [] exportStm(_ _) then Stm
      end
   end

   fun {ShareExp Exp ShareDict}
      case Exp of funExp(Coord String ArgsBodyList) then
	 funExp(Coord String
		{Map ArgsBodyList
		 fun {$ Args#Body} Args#{ShareBody Body ShareDict} end})
      else Exp
      end
   end

   fun {ShareBody Stms ShareDict}
      {Map Stms fun {$ Stm} {ShareStm Stm ShareDict} end}
   end

   fun {TranslateFile File} C VS Program in
      C = {New Compiler.engine init()}
      _ = {New Compiler.interface init(C auto)}
      {C enqueue(setSwitch(expression true))}
      VS = {StockhausenToImperative File}
      {C enqueue(feedVirtualString(VS return(result: ?Program)))}
      {ShareBody Program {NewDictionary}}
   end
end
