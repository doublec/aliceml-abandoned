%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2000
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
      case Stm of valDec(Region IdDef Exp) then
	 valDec(Region IdDef {ShareExp Exp ShareDict})
      [] recDec(Region IdDefExpList) then
	 recDec(Region {Map IdDefExpList
			fun {$ IdDef#Exp} IdDef#{ShareExp Exp ShareDict} end})
      [] refAppDec(_ _ _) then Stm
      [] tupDec(_ _ _) then Stm
      [] prodDec(_ _ _) then Stm
      [] handleStm(Region Body1 IdDef Body2 Body3 Stamp) then
	 handleStm(Region {ShareBody Body1 ShareDict} IdDef
		   {ShareBody Body2 ShareDict} {ShareBody Body3 ShareDict}
		   Stamp)
      [] endHandleStm(_ _) then Stm
      [] testStm(Region Id Tests Body) then
	 testStm(Region Id
		 case Tests of litTests(LitBodyList) then
		    litTests({Map LitBodyList
			      fun {$ Lit#Body}
				 Lit#{ShareBody Body ShareDict}
			      end})
		 [] tagTests(TagBodyList) then
		    tagTests({Map TagBodyList
			      fun {$ Label#N#ConArgs#Body}
				 Label#N#ConArgs#{ShareBody Body ShareDict}
			      end})
		 [] conTests(ConBodyList) then
		    conTests({Map ConBodyList
			      fun {$ Con#ConArgs#Body}
				 Con#ConArgs#{ShareBody Body ShareDict}
			      end})
		 [] vecTests(VecBodyList) then
		    vecTests({Map VecBodyList
			      fun {$ IdDefs#Body}
				 IdDefs#{ShareBody Body ShareDict}
			      end})
		 end
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
