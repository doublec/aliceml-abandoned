%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   Word(make toInt) at 'x-oz://boot/Word'
   Open(file text)
   Property(get)
   System(printInfo)
export
   'UnsafeIO$': IO
define
   Io = {NewUniqueName 'IO.Io'}

   class BaseFile from Open.file Open.text
      attr name
      meth getName($)
	 @name
      end
   end

   TranslateStringFromCRLF TranslateLineFromCRLF
   TranslateStringToCRLF TranslateCharToCRLF

   case {Property.get 'platform.os'} of win32 then
      fun {TranslateStringFromCRLF S}
	 case S of &\r|&\n|Cr then &\n|{TranslateStringFromCRLF Cr}
	 [] C|Cr then C|{TranslateStringFromCRLF Cr}
	 [] "" then ""
	 end
      end

      fun {TranslateLineFromCRLF S}
	 case {Reverse S} of &\r|Rest then {Reverse Rest} else S end
      end

      fun {TranslateStringToCRLF S}
	 case S of &\n|Cr then &\r|&\n|{TranslateStringToCRLF Cr}
	 [] C|Cr then C|{TranslateStringToCRLF Cr}
	 [] "" then ""
	 end
      end

      fun {TranslateCharToCRLF C}
	 case C of &\n then [&\r &\n] else [C] end
      end
   else
      fun {TranslateStringFromCRLF S} S end
      fun {TranslateLineFromCRLF S} S end
      fun {TranslateStringToCRLF S} S end
      fun {TranslateCharToCRLF C} [C] end
   end

   class TextFile from BaseFile
      meth init(name: S flags: F)
	 Open.file, init(name: S flags: text|F)
	 name <- {ByteString.make S}
      end
      meth inputAll($)
	 {ByteString.make Open.file, read(list: $ size: all)}
      end
      meth inputLine($)
	 case TextFile, getS($) of false then {ByteString.make ""}
	 elseof S then {ByteString.make S#'\n'}
	 end
      end
      meth output(S)
	 Open.file, write(vs: S)
      end
      meth output1(C)
	 Open.file, write(vs: [C])
      end
   end

   class StdTextFile from TextFile
      %% Mozart does not open stdin, stdout, and stderr in text mode:
      %% we have to do the translations ourselves.
      meth inputAll($)
	 {ByteString.make {TranslateStringFromCRLF
			   Open.file, read(list: $ size: all)}}
      end
      meth inputLine($)
	 case TextFile, getS($) of false then {ByteString.make ""}
	 elseof S then {ByteString.make {TranslateLineFromCRLF S}#'\n'}
	 end
      end
      meth output(S)
	 Open.file, write(vs: {TranslateStringToCRLF S})
      end
      meth output1(C)
	 Open.file, write(vs: {TranslateCharToCRLF C})
      end
   end

   class BinFile from BaseFile
      meth init(name: S flags: F)
	 Open.file, init(name: S flags: binary|F)
	 name <- {ByteString.make S}
      end
      meth inputAll($)
	 {List.toTuple '#[]'
	  {Map Open.file, read(list: $ size: all)
	   fun {$ C} {Word.make 8 C} end}}
      end
      meth output(S)
	 Open.file, write(vs: {Map {Record.toList S}
			       fun {$ C} {Word.toInt C} end})
      end
      meth output1(C)
	 Open.file, write(vs: [{Word.toInt C}])
      end
   end

   IO =
   'IO'(
      '\'Io': Io
      'Io': fun {$ X} Io(X) end
      'stdIn':
	 {New StdTextFile init(name: stdin flags: [read])}
      'openIn':
	 fun {$ B S}
	    try
	       {New if B then TextFile else BinFile end
		init(name: S flags: [read])}
	    catch system(E=os(os ...) ...) then
	       {Exception.raiseError
		alice(Io(name: S
			 function: {ByteString.make 'openIn'}
			 cause: E))}   %--** cause not of type exn
	       unit
	    end
	 end
      'inputAll':
	 fun {$ F}
	    try {F inputAll($)}
	    catch system(E=os(os ...) ...) then
	       {Exception.raiseError
		alice(Io(name: {F getName($)}
			 function: {ByteString.make 'inputAll'}
			 cause: E))}   %--** cause not of type exn
	       unit
	    end
	 end
      'inputLine':
	 fun {$ F}
	    try {F inputLine($)}
	    catch system(E=os(os ...) ...) then
	       {Exception.raiseError
		alice(Io(name: {F getName($)}
			 function: {ByteString.make 'inputLine'}
			 cause: E))}   %--** cause not of type exn
	       unit
	    end
	 end
      'closeIn':
	 fun {$ F} {F close()} unit end
      'stdOut':
	 {New StdTextFile init(name: stdout flags: [write])}
      'stdErr':
	 {New StdTextFile init(name: stderr flags: [write])}
      'openOut':
	 fun {$ B S}
	    try
	       {New if B then TextFile else BinFile end
		init(name: S flags: [write create truncate])}
	    catch system(E=os(os ...) ...) then
	       {Exception.raiseError
		alice(Io(name: S
			 function: {ByteString.make 'openOut'}
			 cause: E))}   %--** cause not of type exn
	       unit
	    end
	 end
      'openAppend':
	 fun {$ B S}
	    try
	       {New if B then TextFile else BinFile end
		init(name: S flags: [write create append])}
	    catch system(E=os(os ...) ...) then
	       {Exception.raiseError
		alice(Io(name: S
			 function: {ByteString.make 'openAppend'}
			 cause: E))}   %--** cause not of type exn
	       unit
	    end
	 end
      'output':
	 fun {$ F S}
	    try
	       {F output(S)}
	    catch system(E=os(os ...) ...) then
	       {Exception.raiseError
		alice(Io(name: {F getName($)}
			 function: {ByteString.make 'output'}
			 cause: E))}   %--** cause not of type exn
	    end
	    unit
	 end
      'output1':
	 fun {$ F C}
	    try
	       {F output1(C)}
	    catch system(E=os(os ...) ...) then
	       {Exception.raiseError
		alice(Io(name: {F getName($)}
			 function: {ByteString.make 'output1'}
			 cause: E))}   %--** cause not of type exn
	    end
	    unit
	 end
      'flushOut':
	 fun {$ F} /*{F flush()}*/ unit end   %--** not supported for files?
      'closeOut':
	 fun {$ F} {F close()} unit end
      'print':
	 fun {$ X} {System.printInfo X} unit end)
end
