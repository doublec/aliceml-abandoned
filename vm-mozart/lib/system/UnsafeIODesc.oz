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
   OS(open fileDesc read write lSeek close readSelect writeSelect)
export
   'UnsafeIODesc$': IODesc
define
   SysErr = {NewUniqueName 'OS.SysErr'}
   ClosedStream = {NewUniqueName 'IO.ClosedStream'}

   AllPerm = ['S_IRUSR' 'S_IWUSR' 'S_IXUSR'
	      'S_IRGRP' 'S_IWGRP' 'S_IXGRP'
	      'S_IROTH' 'S_IWOTH' 'S_IXOTH']

   class Desc
      prop locking
      attr fd: unit name: unit dir: unit
      meth init(FD Name Dir)
	 fd <- FD
	 name <- Name
	 dir <- Dir
      end
      meth close()
	 lock
	    case @fd of unit then skip
	    elseof FD then
	       try
		  {OS.close FD}
	       catch system(os(_ _ I Text) ...) then
		  {Exception.raiseError
		   alice(SysErr({ByteString.make Text}
				'SOME'({ByteString.make I})))}
	       end
	       fd <- unit
	    end
	 end
      end
      meth hash($)
	 lock
	    case @fd of unit then
	       {Exception.raiseError alice(ClosedStream)} unit
	    elseof FD then FD
	    end
	 end
      end
      meth name($)
	 @name
      end
      meth block()
	 lock
	    case @fd of unit then
	       {Exception.raiseError alice(ClosedStream)}
	    elseof FD then
	       try
		  case @dir of reader then {OS.readSelect FD}
		  [] writer then {OS.writeSelect FD}
		  end
	       catch system(os(_ _ I Text) ...) then
		  {Exception.raiseError
		   alice(SysErr({ByteString.make Text}
				'SOME'({ByteString.make I})))}
	       end
	    end
	 end
      end
      meth setPos(Pos)
	 lock
	    case @fd of unit then
	       {Exception.raiseError alice(ClosedStream)}
	    elseof FD then
	       try
		  {OS.lSeek FD 'SEEK_SET' Pos _}
	       catch system(os(_ _ I Text) ...) then
		  {Exception.raiseError
		   alice(SysErr({ByteString.make Text}
				'SOME'({ByteString.make I})))}
	       end
	    end
	 end
      end
      meth verifyPos($)
	 lock
	    case @fd of unit then
	       {Exception.raiseError alice(ClosedStream)} unit
	    elseof FD then
	       try
		  {OS.lSeek FD 'SEEK_CUR' 0}
	       catch system(os(_ _ I Text) ...) then
		  {Exception.raiseError
		   alice(SysErr({ByteString.make Text}
				'SOME'({ByteString.make I})))} unit
	       end
	    end
	 end
      end
      meth read(I $)
	 lock
	    case @fd of unit then
	       {Exception.raiseError alice(ClosedStream)} unit
	    elseof FD then
	       try
		  {OS.read FD I $ nil _}
	       catch system(os(_ _ I Text) ...) then
		  {Exception.raiseError
		   alice(SysErr({ByteString.make Text}
				'SOME'({ByteString.make I})))} unit
	       end
	    end
	 end
      end
      meth write(Buf $)
	 lock
	    case @fd of unit then
	       {Exception.raiseError alice(ClosedStream)} unit
	    elseof FD then
	       try
		  {OS.write FD Buf $}
	       catch system(os(_ _ I Text) ...) then
		  {Exception.raiseError
		   alice(SysErr({ByteString.make Text}
				'SOME'({ByteString.make I})))} unit
	       end
	    end
	 end
      end
   end

   IODesc =
   'IODesc'('ClosedStream': ClosedStream
	    '\'ClosedStream': ClosedStream

	    %% Common operations supported by all readers/writers
	    'hash': fun {$ Desc} {Desc hash($)} end
	    'compare':
	       fun {$ Desc1 Desc2} I1 I2 in
		  {Desc1 hash(?I1)} {Desc2 hash(?I2)}
		  if I1 < I2 then 'LESS'
		  elseif I1 > I2 then 'GREATER'
		  else 'EQUAL'
		  end
	       end
	    'kind': fun {$ _} 'UNKNOWN' end %--**
	    'name': fun {$ Desc} {Desc name($)} end
	    'chunkSize': fun {$ _} 512 end %--**
	    'close': fun {$ Desc} {Desc close()} unit end

	    %% Common operations not supported by all readers/writers
	    'capabilities':
	       fun {$ _}
		  '#'(block: true
		      setPos: true
		      endPos: false
		      verifyPos: true)
	       end
	    'block': fun {$ Desc} {Desc block()} unit end
	    'setPos': fun {$ Desc Pos} {Desc setPos(Pos)} unit end
	    'endPos': fun {$ _} raise unimplemented end end %--**
	    'verifyPos': fun {$ Desc} {Desc verifyPos($)} end

	    %% Reader operations supported by all readers
	    'avail': fun {$ _} 'NONE' end

	    %% Reader operations not supported by all readers
	    'readerCapabilities':
	       fun {$ Desc}
		  '#'(readVec: true readArr: false
		      readVecNB: false readArrNB: false
		      canInput: false)
	       end
	    'readVec':
	       fun {$ Desc I}
		  {ByteString.make {Desc read(I $)}}
	       end
	    'readArr': fun {$ _} raise unimplemented end end %--**
	    'readVecNB': fun {$ _} raise unimplemented end end %--**
	    'readArrNB': fun {$ _} raise unimplemented end end %--**
	    'canInput': fun {$ _} raise unimplemented end end %--**

	    %% Writer operations not supported by all writers
	    'writerCapabilities':
	       fun {$ _}
		  '#'(writeVec: true writeArr: false
		      writeVecNB: false writeArrNB: false
		      canOutput: false)
	       end
	    'writeVec':
	       fun {$ '#'(iodesc: Desc buf: Buf i: I sz: Sz)}
		  {Desc write({ByteString.slice Buf I I + Sz} $)}
	       end
	    'writeArr': fun {$ _} raise unimplemented end end %--**
	    'writeVecNB': fun {$ _} raise unimplemented end end %--**
	    'writeArrNB': fun {$ _} raise unimplemented end end %--**
	    'canOutput': fun {$ _} raise unimplemented end end %--**

	    %% Creating tty iodescs
	    'stdIn': {New Desc init({OS.fileDesc 'STDIN_FILENO'}
				    {ByteString.make 'stdIn'} reader)}
	    'stdOut': {New Desc init({OS.fileDesc 'STDOUT_FILENO'}
				     {ByteString.make 'stdOut'} writer)}
	    'stdErr': {New Desc init({OS.fileDesc 'STDERR_FILENO'}
				     {ByteString.make 'stdErr'} writer)}

	    %% Creating file iodescs
	    'openIn':
	       fun {$ Name}
		  try
		     {New Desc
		      init({OS.open Name ['O_RDONLY' 'O_NONBLOCK'] nil}
			   Name reader)}
		  catch system(os(_ _ I Text) ...) then
		     {Exception.raiseError
		      alice(SysErr({ByteString.make Text}
				   'SOME'({ByteString.make I})))} unit
		  end
	       end
	    'openOut':
	       fun {$ Name}
		  try
		     {New Desc init({OS.open Name
				     ['O_WRONLY' 'O_CREAT' 'O_TRUNC'
				      'O_NONBLOCK'] AllPerm} Name writer)}
		  catch system(os(_ _ I Text) ...) then
		     {Exception.raiseError
		      alice(SysErr({ByteString.make Text}
				   'SOME'({ByteString.make I})))} unit
		  end
	       end
	    'openAppend':
	       fun {$ Name}
		  try
		     {New Desc init({OS.open Name
				     ['O_WRONLY' 'O_CREAT' 'O_APPEND'
				      'O_NONBLOCK'] AllPerm} Name writer)}
		  catch system(os(_ _ I Text) ...) then
		     {Exception.raiseError
		      alice(SysErr({ByteString.make Text}
				   'SOME'({ByteString.make I})))} unit
		  end
	       end)
end
