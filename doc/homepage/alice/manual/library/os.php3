<?php include("macros.php3"); ?>
<?php heading("The OS structure", "The <TT>OS</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature OS
    structure OS : OS
  </PRE>

  <P>
    The
    <A href="http://www.dina.kvl.dk/~sestoft/sml/os.html">Standard ML
    Basis' <TT>OS</TT></A> structure.
  </P>

  <P>See also:
    <A href="os-file-sys.php3"><TT>OS.FileSys</TT></A>,
    <A href="os-process.php3"><TT>OS.Process</TT></A>,
    <A href="os-io.php3"><TT>OS.IO</TT></A>
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature OS =
    sig
	eqtype syserror
	exception SysErr of string * syserror option

	val errorMsg :      syserror -> string
	val errorName :     syserror -> string
	val syserror :      string -> syserror option

	structure Process : OS_PROCESS
	structure FileSys : OS_FILE_SYS
	structure IO :      OS_IO
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Like the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/os.html">Standard ML
    Basis' <TT>OS</TT></A> structure.
  </P>

  <P>
    <I>Limitations:</I> The following substructure is currently missing:
  </P>

  <UL>
    <LI><TT>Path</TT></LI>
  </UL>

<?php footing() ?>
