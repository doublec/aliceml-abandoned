<?php include("macros.php3"); ?>
<?php heading("The Unix structure", "The <TT>Unix</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature UNIX
    structure Unix : UNIX
  </PRE>

  <P>
    The
    <A href="http://www.dina.kvl.dk/~sestoft/sml/unix.html">Standard ML
    Basis' <TT>Unix</TT></A> structure.
  </P>

  <P>See also:
    <A href="os.php3"><TT>OS</TT></A>
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature UNIX =
    sig
	type proc

	val execute :   string * string list -> proc
	val streamsOf : proc -> TextIO.instream * TextIO.outstream
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Like the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/unix.html">Standard ML
    Basis' <TT>Unix</TT></A> structure.
  </P>

  <P>
    <I>Limitations:</I> The following functionality is currently missing:
  </P>

  <UL>
    <LI><TT>type</TT> <TT>signal</TT></LI>
    <LI><TT>executeInEnv</TT></LI>
    <LI><TT>reap</TT></LI>
    <LI><TT>kill</TT></LI>
  </UL>

<?php footing() ?>
