<?php include("macros.php3"); ?>

<?php heading("Oz for NGWS - Usage", "usage") ?>

<?php section("overview", "overview") ?>
  <P>The interface to the Alice system features:</P>
  <UL>
    <LI><A href="#compiler">the compiler</A>
    <LI><A href="#vm">the virtual machine</A>
  </UL>
  <P>Moreover, Alice provides access to <A href="#foreign">components not
    written in Alice</A>.</P>


<?php section("compiler", "compiler") ?>
  <P>The Alice compiler can be invoked in the following way:</P>
  <DL>
    <DT><TT>stoc</TT> [<TT>-c</TT>]
      <I>&lt;input file&gt;</I> [<TT>-o</TT>] <I>&lt;output file&gt;</I></DT>
    <DD><P>Compile <I>&lt;input file&gt;</I> as an Alice source and write an
      IL file to <I>&lt;output file&gt;</I>.  This then has to be assembled
      by issuing <TT>ilasm /dll </TT><I>&lt;output file&gt;</I>.</P></DD>
  </DL>

  <P>Any imported component <TT>file.dll</TT> must either exist in source
    form (i.e., a file named <TT>file.aml</TT> or <TT>file.sml</TT>)
    or there must exist a file <TT>file.dll.sig</TT>
    which contains the signature of the component (see
    <A href="#foreign">below</A>).
    In both cases the compiler invokes itself recursively to obtain the
    signature of the imported component (since signatures are not yet
    saved persistently).</P>
  <P><A href="libraries.php3#toplevel">Parts of the SML Standard
    Basis top-level environment</A> are automatically available for
    compiling source files.</P>

  <P><B>Important notice:</B>  Due to a bug in the system, it is
    necessary to reassemble some support files each time you invoked
    <TT>stoc</TT>.  To do this, issue the following commands:</P>
  <PRE>
	ilasm /dll TextIO.il
	ilasm /dll Tools.il</PRE>


<?php section("vm", "vm") ?>
  <P>Any component can be invoked as an Alice application.  When
    loading a component, its body is executed; the work of an application is
    performed by its body's side-effects.</P>
  <P>An application is executed by invoking the virtual machine:</P>
  <DL>
    <DT><TT>stow</TT> <I>&lt;application url&gt;</I>
      <I>&lt;args&gt;</I> ...</DT>
    <DD><P>Loads and executes the application given by
      <I>&lt;application url&gt;</I>, denoting a compiled or executable
      component.
  </DL>

  <P>To terminate an application, the <TT>OS.Process.terminate</TT>
    function can be invoked.</P>


<?php section("foreign", "foreign components") ?>

  <P>Alice components can import components that are not written in Alice, see
    <A href="interop.php3#datarepresentation">data representation</A>.
    A component is simply a special DLL.</P>
  <P>However, to import a foreign component <TT>file.dll</TT> one must provide
    a signature file <TT>file.dll.sig</TT> during compilation. This file
    is an ordinary component source file that is restricted to declare
    exactly one signature faithfully specifiying the foreign component.
    The name of the exported signature is irrelevant.</P>


<?php footing() ?>
