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
   Word(make toInt) at 'x-oz://boot/Word'
   Open(file text)
   System(printInfo)
export
   module: IOComponent
define
   IOComponent = tuple(IO)

   Io = {NewUniqueName 'IO.Io'}

   class BaseFile from Open.file Open.text
      attr name
      meth getName($)
	 @name
      end
   end

   class TextFile from BaseFile
      meth init(name: S flags: F)
	 Open.file, init(name: S flags: text|F)
	 name <- {ByteString.make S}
      end
      meth inputAll($)
	 {ByteString.make Open.file, read(list: $ size: all)}
      end
      meth output(S)
	 Open.file, write(vs: S)
      end
      meth output1(C)
	 Open.file, write(vs: [C])
      end
   end

   class BinFile from BaseFile
      meth init(name: S flags: F)
	 Open.file, init(name: S flags: binary|F)
	 name <- {ByteString.make S}
      end
      meth inputAll($)
	 {List.toTuple vector Open.file, read(list: $ size: all)}
      end
      meth output(S)
	 Open.file, write(vs: {Record.toList S})
      end
      meth output1(C)
	 Open.file, write(vs: [C])
      end
   end

   I_PrimeIo    = 1
   I_Io         = 2
   I_closeIn    = 3
   I_closeOut   = 4
   I_flushOut   = 5
   I_inputAll   = 6
   I_inputLine  = 7
   I_openAppend = 8
   I_openIn     = 9
   I_openOut    = 10
   I_output     = 11
   I_output1    = 12
   I_print      = 13
   I_stdErr     = 14
   I_stdIn      = 15
   I_stdOut     = 16

   %--** correctly model blocking

   IO =
   tuple(
      I_PrimeIo: value(Io)
      I_Io: fun {$ X} con(Io X) end#i_v
      I_stdIn:
	 value({New TextFile init(name: stdin flags: [read])})
      I_openIn:
	 fun {$ B S TaskStack}
	    try
	       continue(arg({New if B then TextFile else BinFile end
			     init(name: S flags: [read])}) TaskStack.2)
	    catch system(E=os(os ...) ...) then
	       exception(nil con(Io
				 name: S
				 function: {ByteString.make 'openIn'}
				 cause: E)   %--** cause not of type exn
			 TaskStack.2)
	    end
	 end#rr_t
      I_inputAll:
	 fun {$ F TaskStack}
	    try continue(arg({F inputAll($)}) TaskStack.2)
	    catch system(E=os(os ...) ...) then
	       exception(nil con(Io
				 name: {F getName($)}
				 function: {ByteString.make 'inputAll'}
				 cause: E)   %--** cause not of type exn
			 TaskStack.2)
	    end
	 end#r_t
      I_inputLine:
	 fun {$ F TaskStack}
	    try
	       case {F getS($)} of false then {ByteString.make ""}
	       elseof S then {ByteString.make S#'\n'}
	       end
	    catch system(E=os(os ...) ...) then
	       exception(nil con(Io
				 name: {F getName($)}
				 function: {ByteString.make 'inputLine'}
				 cause: E)   %--** cause not of type exn
			 TaskStack.2)
	    end
	 end#r_t
      I_closeIn:
	 fun {$ F} {F close()} tuple() end#r_v
      I_stdOut:
	 value({New TextFile init(name: stdout flags: [write])})
      I_stdErr:
	 value({New TextFile init(name: stderr flags: [write])})
      I_openOut:
	 fun {$ B S TaskStack}
	    try
	       continue(arg({New if B then TextFile else BinFile end
			     init(name: S flags: [write create truncate])})
			TaskStack.2)
	    catch system(E=os(os ...) ...) then
	       exception(nil con(Io
				 name: S
				 function: {ByteString.make 'openOut'}
				 cause: E)   %--** cause not of type exn
			 TaskStack.2)
	    end
	 end#rr_t
      I_openAppend:
	 fun {$ B S TaskStack}
	    try
	       continue(arg({New if B then TextFile else BinFile end
			     init(name: S flags: [write create append])})
			TaskStack.2)
	    catch system(E=os(os ...) ...) then
	       exception(nil con(Io
				 name: S
				 function: {ByteString.make 'openAppend'}
				 cause: E)   %--** cause not of type exn
			 TaskStack.2)
	    end
	 end#rr_t
      I_output:
	 fun {$ F S TaskStack}
	    try
	       {F output(S)}
	       continue(args() TaskStack.2)
	    catch system(E=os(os ...) ...) then
	       exception(nil con(Io
				 name: {F getName($)}
				 function: {ByteString.make 'output'}
				 cause: E)   %--** cause not of type exn
			 TaskStack.2)
	    end
	 end#rr_t
      I_output1:
	 fun {$ F C TaskStack}
	    try
	       {F output1(C)}
	       continue(args() TaskStack.2)
	    catch system(E=os(os ...) ...) then
	       exception(nil con(Io
				 name: {F getName($)}
				 function: {ByteString.make 'output1'}
				 cause: E)   %--** cause not of type exn
			 TaskStack.2)
	    end
	 end
      I_flushOut:
	 fun {$ F}
	    %{F flush()}   %--** not supported for files?
	    tuple()
	 end#r_v
      I_closeOut:
	 fun {$ F} {F close()} tuple() end#r_v
      I_print:
	 fun {$ X} {System.printInfo X} tuple() end#r_v)
end
