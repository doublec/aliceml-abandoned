functor
import
   Application(getCmdArgs exit)
   System(printError)
   Property(get)
   Main(ozifyFileToStdOut) at 'Main.ozf'
prepare
   Spec = record('in'(single char: &i type: atom optional: false))
define
   Args

   try
      Args = {Application.getCmdArgs Spec}
   catch error(ap(usage M) ...) then
      {System.printError
       'Command line option error: '#M#'\n'#
       'Usage: '#{Property.get 'application.url'}#' [options]\n'#
       '--in=<File>         File containing component to translate.\n'}
      {Application.exit 2}
   end

   {Main.ozifyFileToStdOut Args.'in' _}
   {Application.exit 0}
end
