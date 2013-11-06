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
   Error(exceptionToMessage messageToVirtualString)
   Application(getArgs exit)
   Property(get put)
   Pickle(load saveWithCells)
   Module(apply)
   CodeGen(translate)
   Prebound(builtinTable: BuiltinTable
	    raiseAliceException: RaiseAliceException
	    unwrapAliceException: UnwrapAliceException)
prepare
   OptSpecs = record(help(rightmost char: "h?" default: false))
define
   {Property.put 'alice.builtinTable' BuiltinTable}
   {Property.put 'alice.raiseException' RaiseAliceException}
   {Property.put 'alice.unwrapException' UnwrapAliceException}

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

   fun {ParseDumpTarget Args}
      case Args of "DumpTarget"#OzmFilename|Rest then OzmFilename#Rest
      else unit#Args
      end
   end

   fun {ParsePairList Args ParseX ParseY}
      case Args of X|Y|Rest then
	 {ParseX X}#{ParseY Y}|{ParsePairList Rest ParseX ParseY}
      [] nil then nil
      end
   end

   proc {Loop File}
      case {ReadCommand File} of "Command"#"link"|"Code"#Code|Rest then
	 Filename#AST = {Pickle.load Code}
	 OzmFilename#Rest2 = {ParseDumpTarget Rest}
	 F#VS = {CodeGen.translate Filename AST
		 {ParsePairList Rest2
		  fun {$ "Id"#S} {String.toInt S} end
		  fun {$ "Valref"#S} {Get {String.toInt S}} end}}
      in
	 case OzmFilename of unit then skip
	 else {WriteFile VS OzmFilename}
	 end
	 {System.printInfo 'Valref:'#{Put F}#'\n\n'}
      [] ["Command"#"save" "Valref"#S "String"#OutFilename] then
	 {Pickle.saveWithCells {Get {String.toInt S}} OutFilename '' 9}
	 {System.printInfo '\n'}
      [] nil then
	 {Application.exit 0}
      elseof X then raise format(loop X) end
      end
      {System.gcDo}
      {System.gcDo}
      {Loop File}
   end

   {Property.put 'gc.free' 50}
   try
      {Loop {New TextFile init(name: stdin flags: [read])}}
   catch E then
      V1 = {Error.messageToVirtualString {Error.exceptionToMessage E}}
      V2 = {FoldR {String.tokens {VirtualString.toString V1} &\n}
	    fun {$ S In} 'Error:'#S#'\n'#In end ""}
   in
      {System.printInfo V2#'\n'}
      {Application.exit 1}
   end
end
