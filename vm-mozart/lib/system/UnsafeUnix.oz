%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000-2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Open(pipe text)
export
   'UnsafeUnix$': Unix
define
   class TextPipe from Open.pipe Open.text
      attr cmd
      meth init(cmd: Cmd args: Args)
	 cmd <- Cmd
	 Open.pipe, init(cmd: Cmd args: Args)
      end
      meth getName($)
	 @cmd
      end
      meth inputAll($)
	 {ByteString.make Open.pipe, read(list: $ size: all)}
      end
      meth inputLine($)
	 case TextPipe, getS($) of false then {ByteString.make ""}
	 elseof S then {ByteString.make S#'\n'}
	 end
      end
      meth output(S)
	 Open.pipe, write(vs: S)
      end
      meth output1(C)
	 Open.pipe, write(vs: [C])
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
