<?php include("macros.php3"); ?>

<?php heading("Usage", "usage") ?>

<?php section("overview", "overview") ?>
  <P>The interface to the Alice system features:</P>
  <UL>
    <LI><A href="#interactive">the interactive toplevel</A>
    <LI><A href="#emacs">the interactive toplevel within Emacs</A>
    <LI><A href="#compiler">the batch compiler</A>
    <LI><A href="#vm">the virtual machine</A>
    <LI><A href="#linker">the static linker</A>
  </UL>

<?php section("interactive", "interactive") ?>

  <P>The interactive Alice system is started with the following shell command:
  </P>

  <DL>
    <DT><TT>alice</TT> [<I>&lt;options&gt;</I>]</DT>
  </DL>

  <P>Options are the same as for the standalone
  <A href="#compiler">compiler</A>.</P>

  <P>After preloading several components the system will enter
  an interactive input-eval-output session. You can type in arbitrary
  Alice components. Input can stretch several lines, it is terminated
  by a line containing a semicolon as its last character. As a result
  of your input the system will print the types infered for the given
  declarations, or an appropriate error message. Note that it currently
  does not print any result <I>values</I>. You can use the preloaded
  <A href="inspector.php3">Inspector</A>, however, to comfortably browse
  any results.</P>

  <P>An interactive session can be terminated by typing an end-of-file character
  (Ctrl-D on Unix systems, Ctrl-Z on Windows systems).</P>

  <P>Like in other SML systems, there also is the special purpose function</P>

  <PRE>
	use : string -> unit
  </PRE>

  <P>which, given the name of an Alice source file, will process this
  file as if it had been fed as direct input.</P>

  <P>Each input in the interactive toplevel actually is a
  <A href="components.php3">component</A>. This implies that you
  can use import announcements to link in separately compiled
  components from arbitrary URIs:</P>

  <PRE>
	> import structure Url from "x-alice:/lib/utility/Url";
	### loaded signature from x-alice:/lib/utility/Url
	structure Url : URL = Url
	> 
  </PRE>

  <P>You actually have to use import announcements to access library
  components that are not preloaded, like <TT>Url</TT> above.</P>

  <P>For convenience, import announcements can be abbreviated as follows:</P>

  <PRE>
	> import "x-alice:/lib/utility/Url";
	### loaded signature from x-alice:/lib/utility/Url
	structure Url : URL = Url
	> 
  </PRE>

  <P>This form will import all entities contained in the corresponding
  component.</P>


<?php section("emacs", "emacs") ?>
  <P>Using Emacs with Alice can be done in two steps:</P>
  <UL>
    <LI>Setting up the environment
    <LI>Setting up the Emacs SML mode
  </UL>
  <P>First make sure that the environment variables <TT>OZHOME</TT> and <TT>STOCKHOME</TT>
    point to the installation directory of the Mozart system and the Alice system, respectively.</P>

  <P>Now you have to decide to either use SML mode version 3.3 or SML mode 3.9.3.
    Both of them have their advantages and disadvantages.
    (Note: Within PSL the 3.3 mode is installed in the <TT>site-lisp</TT> directory)</P>

  <P>To use SML mode version 3.3, install this
    <A HREF="sml-mode-3.3.tgz">archive</A>
     either to your global <TT>site-lisp</TT> or to your local <TT>elisp</TT> directory.
     Depending on your selection, add the following
     lines to your <TT>.emacs</TT> file:</P>
  <PRE>
       ;; Necessary only if not already in load-path
       (setq (cons "/this/is/the/mode/directory" load-path))
       ;; Enable sml mode
       (require 'sml-site)
       ;; Enable fontification
       (require 'sml-font)
  </PRE>
  <P>To use SML mode version 3.9.3, extract this
     <A HREF="sml-mode-3.9.3.tgz">archive</A>
     and follow the installation instructions given in
    <TT>INSTALL</TT>. Then invoke <TT>load-library</TT> with argument <TT>sml-proc</TT>.
    Afterwards, invoke <TT>customize-group</TT> with argument <TT>sml</TT> and adjust the
    settings <TT>sml-program-name</TT> to <TT>alice</TT>.</P>


<?php section("compiler", "compiler") ?>
  <P>The stand-alone Alice compiler can be invoked in one of the following
  ways:</P>
  <DL>
    <DT><TT>alicec</TT> [<I>&lt;options&gt;</I>] [<TT>-c</TT>]
      <I>&lt;input file&gt;</I> [<TT>-o</TT> <I>&lt;output file&gt;</I>]</DT>
    <DD><P>Compile <I>&lt;input file&gt;</I> as an Alice source and write a
      pickled component as output.  If <I>&lt;output file&gt;</I> is given,
      use it as the pickle file name, else use the basename of
      <I>&lt;input file&gt;</I> with <TT>.ozf</TT> as extension.</P></DD>
    <DT><TT>alicec</TT> [<I>&lt;options&gt;</I>] <TT>-x</TT>
      <I>&lt;input file&gt;</I> [<TT>-o</TT> <I>&lt;output file&gt;</I>]</DT>
    <DD><P>Compile <I>&lt;input file&gt;</I> as an Alice source and write an
      executable component as output.  If <I>&lt;output file&gt;</I> is given,
      use it as the executable file name, else use the basename of
      <I>&lt;input file&gt;</I> without extension.</P></DD>
    <DT><TT>alicec --replacesign</TT> <I>&lt;input url&gt;</I>
      <I>&lt;signature source&gt;</I> <I>&lt;output file&gt;</I></DT>
    <DD><P>Compile the <I>&lt;signature source&gt;</I>.  Save a compiled
      component to <I>&lt;output file&gt;</I> that contains the component from
      <I>&lt;input url&gt;</I> with the newly compiled signature as export
      signature.</P>
      <P><I>&lt;signature source&gt;</I> must export exactly one signature.
      Its name is irrevelant.</P></DD>
  </DL>
  <P>If an imported component does not exist, but a source file for it
    (i.e., a file with same name but ending in <TT>.aml</TT>, <TT>.sml</TT>,
    or <TT>.sig</TT>) can be located, the compiler first invokes itself
    recursively to compile the imported component.  Note: file modification
    times are not checked.</P>
  <P>Per default, the <A href="libraries.php3#toplevel">SML Standard Basis
    top-level environment</A> is available for compiling source files.</P>
  <P>The following warning options may be given:</P>
  <DL>
    <DT><TT>--(no-)warn-shadowing</TT></DT>
    <DD><P>Whether to warn about shadowing of identifiers.</P></DD>
  </DL>
  <P>The following language options may be given:</P>
  <DL>
    <DT><TT>--no-reexport-import</TT></DT>
    <DD><P>Make imported entities part of the component.</P></DD>
    <DT><TT>--no-implicit-import</TT></DT>
    <DD><P>Do not make the SML Standard Basis top-level environment available
      to source files.  This option is necessary for bootstrapping the
      top-level environment itself.  Improper use will most probably
      crash the compiler.</P></DD>
    <DT><TT>--implicit-import-file</TT> <I>&lt;file&gt;</I></DT>
    <DD><P>Name a file containing import announcements that is automatically
      prepended to any Alice source file before compilation.  Improper use
      will most probably crash the compiler.</P></DD>
    <DT><TT>--rtt-level=no</TT></DT>
    <DD><P>Do not generate code for runtime types.</P></DD>
    <DT><TT>--rtt-level=core</TT></DT>
    <DD><P>Only generate code for core runtime types.</P></DD>
    <DT><TT>--rtt-level=full</TT></DT>
    <DD><P>Full support for runtime types.</P></DD>
  </DL>
  <P>The following debugging options may be given:</P>
  <DL>
    <DT><TT>--version</TT></DT>
    <DD><P>Print compiler version.</P></DD>
    <DT><TT>--(no-)dump-phases</TT></DT>
    <DD><P>Trace the running phases.</P></DD>
    <DT><TT>--(no-)dump-elaboration-sig</TT></DT>
    <DD><P>Output of component signatures.</P></DD>
  </DL>

<?php section("vm", "vm") ?>
  <P>Any compiled component can be invoked as an Alice application.  When
    loading a component, its body is executed; the work of an application is
    performed by its body's side-effects.</P>
  <P>An application is executed either by starting an executable component
    produced by <TT>alic</TT> from the command line or by invoking the
    Virtual Machine directly thus:</P>
  <DL>
    <DT><TT>alicerun</TT> <I>&lt;application url&gt;</I>
      <I>&lt;args&gt;</I> ...</DT>
    <DD><P>Loads and executes the application given by
      <I>&lt;application url&gt;</I>, denoting a compiled or executable
      component.
  </DL>
  <P>The application can access the remaining command line arguments via
    the <A href="libraries.php3#command-line"><TT>CommandLine</TT></A>
    component.  To terminate an application, the <TT>OS.Process.terminate</TT>
    function must be invoked.</P>

<?php section("linker", "linker") ?>

  <P>Synopsis:</P>
  <DL>
    <DT><TT>alicelink</TT> [<I>&lt;options&gt;</I>]
      <I>&lt;input file&gt;</I> ...</DT>
  </DL>

<?php footing() ?>
