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
   Parser(virtualString) at 'x-oz://boot/Parser'
   System(showError)
   Compiler(engine interface)
export
   TranslateVirtualString
define
   local
      fun {Trans X}
	 case X of fAtom(A _) then A
	 [] fInt(I _) then I
	 [] fFloat(F _) then F
	 [] fRecord(Label Args) then ArgCounter in
	    ArgCounter = {NewCell 1}
	    {List.toRecord {Trans Label}
	     {Map Args
	      fun {$ Arg}
		 case Arg of fColon(F T) then
		    {Trans F}#{Trans T}
		 else N NewN in
		    {Exchange ArgCounter ?N NewN}
		    NewN = N + 1
		    N#{Trans Arg}
		 end
	      end}}
	 end
      end
   in
      fun {VirtualStringToValue VS}
	 case {Parser.virtualString VS options(defines: {NewDictionary})}
	 of [ParseTree]#_ then {Trans ParseTree}
	 end
      end
   end

   proc {Share Stamp ShareDict NewStm}
      if {Dictionary.member ShareDict Stamp} then
	 NewStm = {Dictionary.get ShareDict Stamp}
      else
	 {Dictionary.put ShareDict Stamp NewStm}
      end
   end

   fun {ShareStm Stm ShareDict}
      case Stm of valDec(Region Id Exp) then
	 valDec(Region Id {ShareExp Exp ShareDict})
      [] recDec(Region IdExpList) then
	 recDec(Region
		{Map IdExpList fun {$ Id#Exp} Id#{ShareExp Exp ShareDict} end})
      [] evalStm(Region Exp) then evalStm(Region {ShareExp Exp ShareDict})
      [] handleStm(Region Body1 Id Body2 Body3 Stamp) then
	 handleStm(Region {ShareBody Body1 ShareDict} Id
		   {ShareBody Body2 ShareDict} {ShareBody Body3 ShareDict}
		   Stamp)
      [] endHandleStm(_ _) then Stm
      [] testStm(Region Id TestBodyList Body) then
	 testStm(Region Id
		 {Map TestBodyList
		  fun {$ Test#Body} Test#{ShareBody Body ShareDict} end}
		 {ShareBody Body ShareDict})
      [] raiseStm(_ _) then Stm
      [] reraiseStm(_ _) then Stm
      [] sharedStm(Region Body Stamp) then NewStm in
	 {Share Stamp ShareDict NewStm}
	 NewStm = sharedStm(Region {ShareBody Body ShareDict} Stamp)
      [] refStm(Stamp) then
	 {Share Stamp ShareDict}
      [] returnStm(Region Exp) then
	 returnStm(Region {ShareExp Exp ShareDict})
      [] exportStm(_ _) then Stm
      end
   end

   fun {ShareExp Exp ShareDict}
      case Exp of funExp(Region Stamp Flags Args Body) then
	 funExp(Region Stamp Flags Args {ShareBody Body ShareDict})
      else Exp
      end
   end

   fun {ShareBody Stms ShareDict}
      {Map Stms fun {$ Stm} {ShareStm Stm ShareDict} end}
   end

   fun {TranslateVirtualString VS} C in
      C = {New Compiler.engine init()}
      _ = {New Compiler.interface init(C auto)}
      {C enqueue(setSwitch(expression true))}
      case {VirtualStringToValue VS} of InFilename#(Imports#(Body#Sign)) then
	 InFilename#(Imports#({ShareBody Body {NewDictionary}}#Sign))
      end
   end
end
