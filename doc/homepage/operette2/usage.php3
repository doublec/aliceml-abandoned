<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 1 - Usage", "usage") ?>

<?php section("overview", "overview") ?>
  <P>The interface to the Stockhausen system features:</P>
  <UL>
    <LI><A href="#compiler">the compiler</A>
    <LI><A href="#vm">the virtual machine</A>
  </UL>

<?php section("compiler", "compiler") ?>
  <P>The Stockhausen compiler can be invoked in one of the following ways:</P>
  <DL>
    <DT><TT>stoc</TT> [<I>&lt;options&gt;</I>] [<TT>-c</TT>]
      <I>&lt;input file&gt;</I> [<TT>-o</TT> <I>&lt;output file&gt;</I>]</DT>
    <DD><P>Compile <I>&lt;input file&gt;</I> as an Alice source and write a
      pickled component as output.  If <I>&lt;output file&gt;</I> is given,
      use it as the pickle file name, else use the basename of
      <I>&lt;input file&gt;</I> with <TT>.ozf</TT> as extension.</P></DD>
    <DT><TT>stoc</TT> [<I>&lt;options&gt;</I>] <TT>-x</TT>
      <I>&lt;input file&gt;</I> [<TT>-o</TT> <I>&lt;output file&gt;</I>]</DT>
    <DD><P>Compile <I>&lt;input file&gt;</I> as an Alice source and write an
      executable component as output.  If <I>&lt;output file&gt;</I> is given,
      use it as the executable file name, else use the basename of
      <I>&lt;input file&gt;</I> without extension.</P></DD>
    <DT><TT>stoc --replacesign</TT> <I>&lt;input url&gt;</I>
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
  <P>The following extra options may be given:</P>
  <DL>
    <DT><TT>--nodefaultimport</TT></DT>
    <DD><P>Do not make the SML Standard Basis top-level environment available
      to source files.  This option is necessary for bootstrapping the
      top-level environment itself.</P></DD>
    <DT><TT>--outputassembly</TT></DT>
    <DD><P>Write a file <I>&lt;output file&gt;</I><TT>.ozm</TT> containing
      the assembly code for the compiled component.</P></DD>
  </DL>

<?php section("vm", "vm") ?>
  <P>Any compiled component can be invoked as an Alice application.  When
    loading a component, its body is executed; the work of an application is
    performed by its body's side-effects.</P>
  <P>An application is executed either by starting an executable component
    produced by <TT>stoc</TT> from the command line or by invoking the
    Stockhausen Virtual Machine directly thus:</P>
  <DL>
    <DT><TT>stow</TT> <I>&lt;application url&gt;</I>
      <I>&lt;args&gt;</I> ...</DT>
    <DD><P>Loads and executes the application given by
      <I>&lt;application url&gt;</I>, denoting a compiled or executable
      component.
  </DL>
  <P>(<TT>stow</TT> is short for <I>Stockwerk</I>, the name of the virtual
    machine.)</P>
  <P>To terminate an application, the <TT>OS.Process.terminate</TT>
    function must be invoked.</P>

<?php section("installation", "installation") ?>
  <P>In the lab, Stockhausen Operette 1 is installed in the directory</P>
  <PRE>        /opt/stockhausen-operette1/</PRE>
  <P>The <TT>bin</TT> subdirectory has been added to the default <TT>PATH</TT>,
    so that the commands described above are immediately available.</P>

<?php footing() ?>
