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
   Open(file pipe text)
   Property(get)
export
   'UnsafeUnix$': Unix
define
   class BaseFile from Open.file Open.text
      attr name closed: false
      meth getName($)
	 @name
      end
      meth close()
	 Open.file, close()
	 closed <- true
      end
      meth isClosed($)
	 @closed
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
   
   class StdText
      %% Mozart does not open stdin, stdout, and stderr in text mode:
      %% we have to do the translations ourselves.
      meth inputAll($)
	 {ByteString.make {TranslateStringFromCRLF
			   {self read(list: $ size: all)}}}
      end
      meth inputLine($)
	 case TextFile, getS($) of false then {ByteString.make ""}
	 elseof S then {ByteString.make {TranslateLineFromCRLF S}#'\n'}
	 end
      end
      meth output(S)
	 {self write(vs: {TranslateStringToCRLF {VirtualString.toString S}})}
      end
      meth output1(C)
	 {self write(vs: {TranslateCharToCRLF C})}
      end
   end

   class TextPipe from Open.pipe Open.text StdText
      attr cmd
      meth init(cmd: Cmd args: Args)
	 cmd <- Cmd
	 Open.pipe, init(cmd: Cmd args: Args)
      end
      meth getName($)
	 @cmd
      end
   end

   Unix =
   'Unix'('execute':
	     fun {$ Cmd Args} P in
		P = {New TextPipe init(cmd: Cmd args: Args)}
		P#P
	     end
	  'streamsOf':
	     fun {$ Instream#Outstream}
		Instream#Outstream
	     end)
end
