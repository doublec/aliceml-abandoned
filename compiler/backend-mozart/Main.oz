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
   Open(file text)
   System(printInfo printError gcDo)
   Application(getArgs exit)
   Property(get put)
   Pickle(load saveWithCells)
   Module(apply)
   CodeGen(translate)
prepare
   OptSpecs = record(help(rightmost char: "h?" default: false))
define
   class TextFile from Open.file Open.text end

   fun {ReadCommand File}
      case {File getS($)} of false then nil
      [] "" then nil
      elseof S then Name in
	 case {List.takeDropWhile S fun {$ C} C \= &: end ?Name}
	 of &:|Rest then
	    Name#{List.dropWhile Rest Char.isSpace}|{ReadCommand File}
	 else raise format(readCommand S) end
	 end
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
       'Usage: '#{Property.get 'application.url'}#' [--help]\n'}
      {Application.exit 0}
   end

   proc {WriteFile VS File} F in
      F = {New Open.file init(name: File flags: [write create truncate])}
      {F write(vs: VS)}
      {F close()}
   end

   fun {ParsePairList Args ParseX ParseY}
      case Args of X|Y|Rest then
	 {ParseX X}#{ParseY Y}|{ParsePairList Rest ParseX ParseY}
      [] nil then nil
      end
   end

   proc {Loop File}
      case {ReadCommand File} of "Command"#"link"|"Code"#Code|Rest then
	 case {Pickle.load Code} of unit then
	    {System.printInfo 'Error:could not load pickle\n\n'}
	 [] Filename#AST then
	    case {CodeGen.translate Filename AST
		  {ParsePairList Rest
		   fun {$ "Id"#S} {String.toInt S} end
		   fun {$ "Valref"#S} {Get {String.toInt S}} end}}
	    of F#VS then
	       {WriteFile VS Filename#'.ozm'}
	       {System.printInfo 'Valref:'#{Put F}#'\n\n'}
	    end
	 end
      [] ["Command"#"save" "Valref"#S "String"#OutFilename] then
	 {Pickle.saveWithCells {Get {String.toInt S}} OutFilename '' 9}
	 {System.printInfo '\n'}
      [] "Command"#"apply"|"Valref"#S|Rest then
	 case {Get {String.toInt S}} of F then M in
	    [M] = {Module.apply [F]}
	    {List.forAll Rest
	     proc {$ "Label"#S}
		{System.printInfo 'Valref:'#{Put M.{String.toAtom S}}#'\n'}
	     end}
	    {System.printInfo '\n'}
	 end
      [] nil then
	 {Application.exit 0}
      elseof X then raise format(loop X) end
      end
      {System.gcDo}
      {System.gcDo}
      {Loop File}
   end

   {Property.put 'gc.free' 50}
   {Loop {New TextFile init(name: stdin flags: [read])}}
end
