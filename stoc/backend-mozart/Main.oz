%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Application(getCmdArgs exit)
   Pickle(save)
   System(printError)
   Property(get)
   Frontend(translateFile)
   CodeGen(translate)
prepare
   Spec = record('in'(single char: &i type: string optional: false)
		 'out'(single char: &o type: string optional: false)
		 'image'(single type: string default: "../top/stoc-mozart"))
define
   Args

   try
      Args = {Application.getCmdArgs Spec}
   catch error(ap(usage M) ...) then
      {System.printError
       'Command line option error: '#M#'\n'#
       'Usage: '#{Property.get 'application.url'}#' [options]\n'#
       '--in=<File>         File containing component to translate.\n'#
       '--out=<File>        Name of pickle to write.\n'#
       '--image=<File>      Where to locate the SML/NJ heap image.\n'}
      {Application.exit 2}
   end

   case {Frontend.translateFile Args.'in' Args.'image'} of unit then
      {Application.exit 1}
   elseof AST then
      {Pickle.save {CodeGen.translate AST} Args.'out'}
      {Application.exit 0}
   end
end
