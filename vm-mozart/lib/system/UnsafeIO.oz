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
	 {New TextFile init(name: stdin flags: [read])}
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
	    try
	       case {F getS($)} of false then {ByteString.make ""}
	       elseof S then {ByteString.make S#'\n'}
	       end
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
	 {New TextFile init(name: stdout flags: [write])}
      'stdErr':
	 {New TextFile init(name: stderr flags: [write])}
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
