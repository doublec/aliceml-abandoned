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
   OzOS(chDir getCWD mkDir stat unlink tmpnam system getEnv)
   at 'x-oz://system/OS.ozf'
   Property(put)
   Error(printException)
   System(onToplevel)
   Application(exit)
export
   module: OSComponent
define
   OSComponent = tuple(UnsafeOS)

   NONE = 0
   SOME = 1

   I_PrimeSysErr = 1
   I_FileSys     = 2
   I_Process     = 3
   I_SysErr      = 4

   I_chDir    = 1
   I_fileSize = 2
   I_getDir   = 3
   I_isDir    = 4
   I_mkDir    = 5
   I_modTime  = 6
   I_remove   = 7
   I_tmpName  = 8

   I_atExn   = 1
   I_exit    = 2
   I_failure = 3
   I_getEnv  = 4
   I_success = 5
   I_system  = 6

   SysErr = {NewUniqueName 'OS.SysErr'}

   FileSys =
   tuple(I_chDir:
	    fun {$ Name TaskStack}
	       try
		  {OzOS.chDir Name}
		  continue(args() TaskStack.2)
	       catch system(os(os ...) ...) then
		  exception(nil
			    con(SysErr {ByteString.make
					'chDir: cannot change directory'}
				NONE) TaskStack.2)
	       end
	    end#r_t
	 I_getDir: fun {$} {ByteString.make {OzOS.getCWD}} end#n_v
	 I_mkDir:
	    fun {$ Name TaskStack}
	       try
		  {OzOS.mkDir Name ['S_IRUSR' 'S_IWUSR' 'S_IXUSR'
				    'S_IRGRP' 'S_IWGRP' 'S_IXGRP'
				    'S_IROTH' 'S_IWOTH' 'S_IXOTH']}
		  continue(args() TaskStack.2)
	       catch system(os(os ...) ...) then
		  exception(nil
			    con(SysErr {ByteString.make
					'mkDir: cannot create directory'}
				NONE) TaskStack.2)
	       end
	    end#r_t
	 I_isDir:
	    fun {$ Name TaskStack}
	       try
		  continue(arg(if {OzOS.stat Name}.type == dir
			       then 1 else 0 end) TaskStack.2)
	       catch system(os(os ...) ...) then
		  exception(nil
			    con(SysErr {ByteString.make
					'isDir: cannot get file attributes'}
				NONE) TaskStack.2)
	       end
	    end#r_t
	 I_fileSize:
	    fun {$ Name TaskStack}
	       try
		  continue(arg({OzOS.stat Name}.size) TaskStack.2)
	       catch system(os(os ...) ...) then
		  exception(nil
			    con(SysErr {ByteString.make
					'fileSize: cannot get file size'}
				NONE) TaskStack.2)
	       end
	    end#r_t
	 I_modTime:
	    fun {$ Name TaskStack}
	       try
		  continue(arg({OzOS.stat Name}.mtime * 1000000) TaskStack.2)
	       catch system(os(os ...) ...) then
		  exception(nil
			    con(SysErr {ByteString.make
					'modTime: cannot get file time'}
				NONE) TaskStack.2)
	       end
	    end#r_t
	 I_remove:
	    fun {$ Name TaskStack}
	       try
		  continue(arg({OzOS.unlink Name}) TaskStack.2)
	       catch system(os(os ...) ...) then
		  exception(nil
			    con(SysErr {ByteString.make
					'remove: cannot remove file'}
				NONE) TaskStack.2)
	       end
	    end#r_t
	 I_tmpName: fun {$} {ByteString.make {OzOS.tmpnam}} end#n_v)


   Process =
   tuple(I_success: value(0)
	 I_failure: value(1)
	 I_system: OzOS.system#r_v
	 I_atExn:
	    fun {$ Closure}
	       {Property.put 'alice.atExn' Closure}
	       tuple()
	    end#r_v
	 I_exit: proc {$ N _} {Application.exit N} end#r_v
	 I_getEnv:
	    fun {$ S}
	       case {OzOS.getEnv S} of false then NONE
	       elseof S2 then tag(SOME {ByteString.make S2})
	       end
	    end#r_v)

   UnsafeOS = tuple(I_PrimeSysErr: SysErr
		    I_SysErr: fun {$ A B} con(SysErr A B) end#ii_v
		    I_FileSys: FileSys
		    I_Process: Process)
end
