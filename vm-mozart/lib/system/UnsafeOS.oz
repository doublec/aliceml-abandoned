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
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   OzOS(chDir getCWD mkDir stat unlink tmpnam system getEnv)
   at 'x-oz://system/OS.ozf'
   Property(put)
   Error(printException)
   System(onToplevel)
   Application(exit)
export
   'UnsafeOS$': OS
define
   SysErr = {NewUniqueName 'OS.SysErr'}

   OS =
   'OS'('\'SysErr': SysErr
	'SysErr': fun {$ A B} SysErr(A B) end
	'errorMsg': fun {$ X} {ByteString.make 'Unknown error code '#X} end
	'FileSys$':
	   'FileSys'('chDir':
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
			   {Property.put 'errors.handler'
			    proc {$ E}
			       case E of system(kernel(terminate) ...) then
				  skip
			       [] error(alice(Exn ...) ...) then
				  _ = {P Exn}
			       else
				  {Error.printException E}
				  if {System.onToplevel} then
				     {Application.exit 1}
				  end
			       end
			    end}
			   unit
			end
		     'exit':
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
