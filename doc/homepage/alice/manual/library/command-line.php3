<?php include("macros.php3"); ?>
<?php heading("The CommandLine structure", "The <TT>CommandLine</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature COMMAND_LINE
    structure CommandLine : COMMAND_LINE
  </PRE>

  <P>
    The
    <A href="http://www.dina.kvl.dk/~sestoft/sml/command-line.html">Standard ML
    Basis' <TT>CommandLine</TT></A> structure.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature COMMAND_LINE =
    sig
	val name :      unit -> string
	val arguments : unit -> string list
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Like the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/command-line.html">Standard ML
    Basis' <TT>CommandLine</TT></A> structure.
  </P>

<?php footing() ?>
