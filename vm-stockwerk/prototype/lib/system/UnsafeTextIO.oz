%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   Open(file text)
   Property(get)
   System(printInfo)
export
   module: TextIOComponent
define
   TextIOComponent = tuple(TextIO)

   IoException = {NewUniqueName 'IO.Io'}

   class TextFile from Open.file Open.text end

   ConvertLine ConvertAll
   case {Property.get 'platform.os'} of win32 then
      fun {ConvertLine S}
	 case S of [&\r] then nil
	 [] C|Cr then C|{ConvertLine Cr}
	 [] nil then nil
	 end
      end
      fun {ConvertAll S}
	 case S of &\r|&\n|Rest then &\n|{ConvertAll Rest}
	 [] C|Cr then C|{ConvertAll Cr}
	 [] nil then nil
	 end
      end
   else
      fun {ConvertLine S} S end
      fun {ConvertAll S} S end
   end

   fun {InputAll F}
      {ConvertAll {F read(list: $ size: all)}}
   end

   I_closeIn   = 1
   I_closeOut  = 2
   I_flushOut  = 3
   I_inputAll  = 4
   I_inputLine = 5
   I_openIn    = 6
   I_openOut   = 7
   I_output    = 8
   I_output1   = 9
   I_print     = 10
   I_stdErr    = 11
   I_stdIn     = 12
   I_stdOut    = 13

   I_cause    = 2
   I_function = 3
   I_name     = 4

   %--** lots of IO exceptions are not handled below:

   TextIO =
   tuple(
      I_stdIn:
	 value({New TextFile init(name: stdin flags: [read])})
      I_openIn:
	 fun {$ S TaskStack}
	    try
	       continue(arg({New TextFile init(name: S flags: [read])})
			TaskStack.2)
	    catch system(E=os(os ...) ...) then
	       exception(nil con(IoException
				 I_name: S
				 I_function: {ByteString.make 'openIn'}
				 I_cause: E)   %--** cause not of type exn
			 TaskStack.2)
	    end
	 end#r_t
      I_inputAll:
	 fun {$ F} {ByteString.make {InputAll F}} end#r_v
      I_inputLine:
	 fun {$ F}
	    case {F getS($)} of false then {ByteString.make ""}
	    elseof S then {ByteString.make {ConvertLine S}#'\n'}
	    end
	 end#r_v
      I_closeIn: fun {$ F} {F close()} tuple() end#r_v
      I_stdOut: value({New TextFile init(name: stdout flags: [write])})
      I_stdErr: value({New TextFile init(name: stderr flags: [write])})
      I_openOut:
	 fun {$ S}
	    {New TextFile init(name: S flags: [write create truncate])}
	 end#r_v
      I_output:
	 fun {$ F S} {F write(vs: S)} tuple() end#rr_v
      I_output1:
	 fun {$ F C} {F write(vs: [C])} tuple() end#rr_v
      I_flushOut:
	 fun {$ F}
	    %{F flush()}   %--** not supported for files?
	    tuple()
	 end#r_v
      I_closeOut: fun {$ F} {F close()} tuple() end#r_v
      I_print: fun {$ X} {System.printInfo X} tuple() end#r_v)
end
