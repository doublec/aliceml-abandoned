%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt & Andreas Rossberg, 2000-2003
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   OzOS(getDir chDir getCWD mkDir stat unlink tmpnam system getEnv)
   at 'x-oz://system/OS.ozf'
   Property(get)
   Application(exit)
export
   'UnsafeOS$': OS
define
   SysErr = {NewUniqueName 'OS.SysErr'}

   fun {ReadDir Ss L}
      case Ss
      of closed then L = closed
		     {Exception.raiseError
		      alice(SysErr('openDir: cannot read directory' 'NONE'))}
		     unit
      [] nil then L = nil 'NONE'
      [] "."|Ss2 then {ReadDir Ss2 L}
      [] ".."|Ss2 then {ReadDir Ss2 L}
      [] S|Ss2 then L = Ss2 'SOME'({ByteString.make S})
      end
   end

   OS =
   'OS'('\'SysErr': SysErr
	'SysErr': fun {$ A B} SysErr(A B) end
	'errorMsg': fun {$ X} {ByteString.make 'Unknown error code '#X} end
	'FileSys$':
	   'FileSys'('openDir':
			fun {$ Name}
			   try
			      Name#{NewCell {OzOS.getDir Name}}
			   catch system(os(os ...) ...) then
			      {Exception.raiseError
			       alice(SysErr('openDir: cannot read directory'
					    'NONE'))}
			      unit
			   end
			end
		     'rewindDir':
			fun {$ Name#C} L in
			   case {Exchange C $ L}
			   of closed then
			      L = closed
			      {Exception.raiseError
			       alice(SysErr('rewindDir: directory closed'
					    'NONE'))}
			   [] _ then
			      try
				 L = {OzOS.getDir Name}
			      catch system(os(os ...) ...) then
				L = nil
				{Exception.raiseError
				 alice(SysErr('rewindDir: cannot read directory'
					      'NONE'))}
			      end
			   end
			   unit
			end
		     'closeDir':
			fun {$ _#C}
			   {Assign C closed}
			   unit
			end
		     'readDir':
			fun {$ _#C} L in
			   {ReadDir {Exchange C $ L} L}
			end
		     'chDir':
			fun {$ Name}
			   try
			      {OzOS.chDir Name}
			   catch system(os(os ...) ...) then
			      {Exception.raiseError
			       alice(SysErr('chDir: cannot change directory'
					    'NONE'))}
			   end
			   unit
			end
		     'getDir':
			fun {$ unit}
			   {ByteString.make {OzOS.getCWD}}
			end
		     'mkDir':
			fun {$ Name}
			   try
			      {OzOS.mkDir Name ['S_IRUSR' 'S_IWUSR' 'S_IXUSR'
						'S_IRGRP' 'S_IWGRP' 'S_IXGRP'
						'S_IROTH' 'S_IWOTH' 'S_IXOTH']}
			   catch system(os(os ...) ...) then
			      {Exception.raiseError
			       alice(SysErr('mkDir: cannot create directory'
					    'NONE'))}
			   end
			   unit
			end
		     'rmDir':
			fun {$ Name}
			   {Exception.raiseError notImplemented} unit %--**
			end
		     'isDir':
			fun {$ Name}
			   try
			      {OzOS.stat Name}.type == dir
			   catch system(os(os ...) ...) then
			      {Exception.raiseError
			       alice(SysErr('isDir: cannot get file attributes'
					    'NONE'))}
			      unit
			   end
			end
		     'isLink':
			fun {$ Name}
			   {Exception.raiseError notImplemented} unit %--**
			end
		     'readLink':
			fun {$ Name}
			   {Exception.raiseError notImplemented} unit %--**
			end
		     'fileSize':
			fun {$ Name}
			   try
			      {OzOS.stat Name}.size
			   catch system(os(os ...) ...) then
			      {Exception.raiseError
			       alice(SysErr('fileSize: cannot get file size'
					    'NONE'))}
			      unit
			   end
			end
		     'modTime':
			fun {$ Name}
			   try
			      {OzOS.stat Name}.mtime * 1000000
			   catch system(os(os ...) ...) then
			      {Exception.raiseError
			       alice(SysErr('modTime: cannot get file time'
					    'NONE'))}
			      unit
			   end
			end
		     'remove':
			fun {$ Name}
			   try
			      {OzOS.unlink Name}
			   catch system(os(os ...) ...) then
			      {Exception.raiseError
			       alice(SysErr('remove: cannot remove file'
					    'NONE'))}
			   end
			   unit
			end
		     'tmpName':
			fun {$ unit}
			   {ByteString.make {OzOS.tmpnam}}
			end)
	'Process$':
	   'Process'('success': 0
		     'failure': 1
		     'system': OzOS.system
		     'atExn':
			fun {$ P}
			   PS2
			   PS = {Exchange
				 {Property.get 'alice.atExnActions'} $ PS2}
			in
			   PS2 = P|PS
			   unit
			end
		     'terminate':
			proc {$ N _}
			   {Application.exit N}
			end
		     'getEnv':
			fun {$ S}
			   case {OzOS.getEnv S} of false then 'NONE'
			   elseof S2 then 'SOME'({ByteString.make S2})
			   end
			end))
end
