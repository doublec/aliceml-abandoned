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
   Open(file text)
   System(printInfo)
export
   'UnsafeIO$': IO
define
   Io = {NewUniqueName 'IO.Io'}

   class TextFile from Open.file Open.text
      attr Name
      meth init(name: S flags: F)
	 Open.file, init(name: S flags: F)
	 Name <- {ByteString.make S}
      end
      meth getName($)
	 @Name
      end
   end

   IO =
   'IO'(
      '\'Io': Io
      'Io': fun {$ X} Io(X) end
      'stdIn':
	 {New TextFile init(name: stdin flags: [read text])}
      'openIn':
	 fun {$ B S}
	    try
	       {New TextFile init(name: S
				  flags: [read
					  if B then text else binary end])}
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
	    try {ByteString.make {F read(list: $ size: all)}}
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
	 {New TextFile init(name: stdout flags: [write text])}
      'stdErr':
	 {New TextFile init(name: stderr flags: [write text])}
      'openOut':
	 fun {$ B S}
	    try
	       {New TextFile init(name: S
				  flags: [write create truncate
					  if B then text else binary end])}
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
	       {New TextFile init(name: S
				  flags: [write create append
					  if B then text else binary end])}
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
	       {F write(vs: S)}
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
	       {F write(vs: [C])}
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
