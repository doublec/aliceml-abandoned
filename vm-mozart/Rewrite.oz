%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Application(getArgs exit)
   Property(get)
   System(printError printInfo)
   Open(file)
   Pickle(load saveWithCells)
define
   fun {StringPair S} From in
      case {List.takeDropWhile S fun {$ C} C \= &= end ?From}
      of &=|To then From#To
      [] nil then
	 {Exception.raiseError
	  ap(usage 'expected rewrite rule of the form <from>=<to>')} unit
      end
   end

   Spec = record(help(leftmost char: [&h &?] default: false)
		 verbose(leftmost char: [&v] default: false)
		 rewrite(multiple char: &r type: list(StringPair) default: nil)
		 outputfile(single char: &o type: string default: unit)
		 executable(rightmost char: &x type: bool default: false)
		 execheader(single type: string
			    validate:
			       alt(when(disj(execpath execfile execwrapper)
					false)))
		 execpath(single type: string
			  validate:
			     alt(when(disj(execheader execfile execwrapper)
				      false)))
		 execfile(single type: string
			  validate:
			     alt(when(disj(execheader execpath execwrapper)
				      false)))
		 execwrapper(single type: string
			     validate:
				alt(when(disj(execheader execpath execfile)
					 false)))
		 compress(rightmost char: &z
			  type: int(min: 0 max: 9) default: 0))

   proc {Usage Status}
      {System.printError
       'Usage: '#{Property.get 'application.url'}#
       '[options] <inputfile> [options]\n'#
       'Options:\n'#
       '--help, --usage, -h, -?\n' #
       '\tPrint this message.\n' #
       '--(no)verbose (default: false)\n' #
       '\tPrint messages on activities performed.\n' #
       '--rewrite=RULE,...,RULE, -r RULE,...,RULE\n'#
       '\twhere a RULE of the form FROM=TO specifies\n'#
       '\thow to replace URI prefixes.\n'#
       '--outputfile=FILE, -o FILE\n'#
       '\tspecifies where to write the pickle\n'#
       '\twith its import URIs rewritten.\n'#
       '--execheader=STR\n' #
       '\tUse header STR for executables\n' #
       '\t(Unix default: "#!/bin/sh\\nexec ozengine $0 "$@"\\n").\n'#
       '--execpath=STR\n' #
       '\tUse above header, with ozengine replaced by STR.\n'#
       '--execfile=FILE\n' #
       '\tUse contents of FILE as header\n' #
       '\t(Windows default: <ozhome>/bin/ozwrapper.bin).\n' #
       '--execwrapper=FILE\n' #
       '\tUse above header, with ozwrapper.bin replaced by STR.\n' #
       '--compress=N, -z N (N: 0..9, default 0)\n' #
       '\tUse compression level N for created pickle.\n'}
      {Application.exit Status}
   end

   proc {CommandLineError M}
      {System.printError
       'Command line option error: '#M#'\n'}
      {Usage 2}
   end

   Opts = try
	     {Application.getArgs Spec}
	  catch error(ap(usage M) ...) then
	     {CommandLineError M}
	     unit
	  end

   fun {MakeExecHeader Path}
      '#!/bin/sh\nexec '#Path#' $0 "$@"\n'
   end
   fun {MakeExecFile File}
      {Property.get 'oz.home'}#'/bin/'#File
   end
   DefaultExec = case {Property.get 'platform.os'} of win32 then
		    file({MakeExecFile 'ozwrapper.bin'})
		 else
		    string({MakeExecHeader 'ozengine'})
		 end

   proc {ReadFile File ?VS} F in
      F = {New Open.file init(name: File flags: [read])}
      {F read(list: ?VS size: all)}
      {F close()}
   end

   fun {RewriteString S Rules}
      case Rules of From#To|Rest then
	 if {List.isPrefix From S} then
	    To#{List.drop S {Length From}}
	 else {RewriteString S Rest}
	 end
      [] nil then S
      end
   end

   fun {RewriteFrom Url Rules Verbose} NewUrl in
      NewUrl = {VirtualString.toAtom
		{RewriteString {VirtualString.toString Url} Rules}}
      if Verbose then
	 {System.printInfo Url#'\n\t=> '#NewUrl#'\n'}
      end
      NewUrl
   end

   fun {RewriteImport Import Rules Verbose}
      {Record.map Import
       fun {$ Info}
	  {AdjoinAt Info 'from' {RewriteFrom Info.'from' Rules Verbose}}
       end}
   end

   if Opts.help then {Usage 0}
   elsecase Opts.1 of [InputFilename] then
      case Opts.outputfile of unit then
	 {CommandLineError 'output file name not given.'}
      elseof OutputFilename then F in
	 try
	    F = {Pickle.load InputFilename}
	 catch _ then
	    {CommandLineError 'could not load input file.'}
	 end
	 if {Functor.is F} then
	    NewF = {Functor.new
		    {RewriteImport F.'import' Opts.rewrite Opts.verbose}
		    F.'export' F.apply}
	 in
	    {Pickle.saveWithCells NewF OutputFilename
	     if Opts.executable then
		Exec = case {CondSelect Opts execheader unit}
		       of unit then
			  case {CondSelect Opts execpath unit}
			  of unit then
			     case {CondSelect Opts execfile unit}
			     of unit then
				case {CondSelect Opts execwrapper unit}
				of unit then
				   DefaultExec
				elseof S then file({MakeExecFile S})
				end
			     elseof S then file(S)
			     end
			  elseof S then string({MakeExecHeader S})
			  end
		       elseof S then string(S)
		       end
	     in
		case Exec of file(S) then {ReadFile S}
		[] string(S) then S
		end
	     else ''
	     end
	     Opts.compress}
	 else
	    {CommandLineError 'input file is not a functor.'}
	 end
	 {Application.exit 0}
      end
   else {CommandLineError 'none or multiple input files given.'}
   end
end
