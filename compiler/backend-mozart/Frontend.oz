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
   Open(text pipe)
   System(showError)
   Compiler(engine interface)
export
   TranslateFile
define
   class TextPipe from Open.pipe Open.text
      meth get($)
	 case TextPipe, getS($) of S=&(|_ then S#'\n'
	 [] false then false
	 elseof S then
	    {System.showError S}
	    TextPipe, get($)
	 end
      end
   end

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

   fun {StockhausenToImperative File Image} Pipe S in
      Pipe = {New TextPipe
	      init(cmd: 'sml-cm'
		   args: ['@SMLload='#Image File])}
      {Pipe get(?S)}
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
      [] evalStm(Coord Exp) then evalStm(Coord {ShareExp Exp ShareDict})
      [] handleStm(Coord Body1 Id Body2 Body3 I) then X in
	 X = {NewCell true}
	 {Share I ShareDict X}
	 handleStm(Coord {ShareBody Body1 ShareDict} Id
		   {ShareBody Body2 ShareDict} {ShareBody Body3 ShareDict} X)
      [] endHandleStm(Coord I) then
	 endHandleStm(Coord {Share I ShareDict})
      [] testStm(Coord Id Test Body1 Body2) then
	 testStm(Coord Id Test
		 {ShareBody Body1 ShareDict} {ShareBody Body2 ShareDict})
      [] raiseStm(_ _) then Stm
      [] reraiseStm(_ _) then Stm
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
      case Exp of funExp(Coord Stamp Flags Args Body) then
	 funExp(Coord Stamp Flags Args {ShareBody Body ShareDict})
      else Exp
      end
   end

   fun {ShareBody Stms ShareDict}
      {Map Stms fun {$ Stm} {ShareStm Stm ShareDict} end}
   end

   fun {TranslateFile File Image} C in
      C = {New Compiler.engine init()}
      _ = {New Compiler.interface init(C auto)}
      {C enqueue(setSwitch(expression true))}
      case {StockhausenToImperative File Image} of false then unit
      elseof VS then Imports Body in
	 Imports#Body = {VirtualStringToValue VS}
	 Imports#{ShareBody Body {NewDictionary}}
      end
   end
end
