%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
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
   Open(file text)
   System(printInfo printError)
   Application(getArgs exit)
   Property(get)
   Pickle(save)
   Frontend(translateVirtualString)
   CodeGen(translate)
prepare
   OptSpecs = record(help(rightmost char: "h?" default: false)
		     debug(rightmost default: false))
define
   class TextFile from Open.file Open.text end

   fun {ReadMessage File}
      case {File getS($)} of false then nil
      [] "" then nil
      elseof S then Name in
	 case {List.takeDropWhile S fun {$ C} C \= &: end ?Name}
	 of &:|Rest then
	    Name#{List.dropWhile Rest Char.isSpace}|{ReadMessage File}
	 else raise format(readMessage S) end
	 end
      end
   end

   fun {ReadCommand File}
      case {ReadMessage File} of "Command"#Command|Arguments then
	 {String.toAtom Command}#
	 {List.map Arguments
	  fun {$ Name#Value}
	     case Name of "Argument" then Value
	     else raise format end
	     end
	  end}
      [] nil then unit
      else raise format(readCommand) end
      end
   end

   local
      Values = {Dictionary.new}

      Cell = {NewCell 0}
   in
      fun {Put V} Id in
	 Id = {Access Cell}
	 {Assign Cell Id + 1}
	 {Dictionary.put Values Id V}
	 Id
      end

      fun {Get Id}
	 {Dictionary.get Values Id}
      end
   end

   OptRec = try
	       {Application.getArgs OptSpecs}
	    catch error(ap(usage VS) ...) then
	       {System.printError
		{Property.get 'application.url'}#': '#VS#'\n'}
	       {Application.exit 2}
	       unit
	    end

   if OptRec.help then
      {System.printError
       'Usage: '#{Property.get 'application.url'}#' [--help] [--debug]\n'}
      {Application.exit 0}
   end

   proc {Loop File}
      case {ReadCommand File} of 'buildFunctor'#[Code] then
	 case {Frontend.translateVirtualString Code} of unit then
	    {System.printInfo 'Result: ~1\n\n'}
	 elseof AST then Id in
	    Id = {Put {CodeGen.translate AST OptRec.debug}}
	    {System.printInfo 'Result: '#Id#'\n\n'}
	 end
      [] 'saveValue'#[OutFilename Id] then
	 {Pickle.save {Get {String.toInt Id}} OutFilename}
	 {System.printInfo 'Result: 0\n\n'}
      [] unit then
	 {Application.exit 0}
      elseof X then raise format(loop X) end
      end
      {Loop File}
   end

   {Loop {New TextFile init(name: stdin flags: [read])}}
end
