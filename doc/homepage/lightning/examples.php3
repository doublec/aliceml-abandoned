<?php include("macros.php3"); ?>

<?php heading("Oz for NGWS - Examples", "examples") ?>



<?php section("streams", "streams") ?>

  <P>
    This program allows the user to interactively put numbers on two streams.
    Several other streams are concurrently produced by different combinations
    of those two streams (addition, zipping, mapping, etc.). All streams are
    displayed in the Inspector.
  </P>

  <UL>
    <LI> <A href="../examples/streams.aml"><TT>streams.aml</TT></A> </LI>
  </UL>

  <P>This demo is also provided in <A href="../">precompiled form</A>.
    It can be started by issuing the command:</P>
  <PRE>
	stow streams.dll</PRE>
  <P>Two windows are displayed:  The inspector window,
    entitled&nbsp;<TT>A</TT>, displaying a record with the
    following fields:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Label<TH>Value
      <TR>
	<TD><TT>A</TT>
	<TD>Input stream #1.
      <TR>
	<TD><TT>B</TT>
	<TD>Input stream #2.
      <TR>
	<TD><TT>AplusB</TT>
	<TD>An integer lazy stream summing up elements in streams #1 and #2.
      <TR>
	<TD><TT>AtoChar</TT>
	<TD>The character stream interpreting each integer from stream #1
	  as a character.
      <TR>
	<TD><TT>AzipB</TT>
	<TD>An integer stream accumulating the elements from streams #1 and #2.
      <TR>
	<TD><TT>Bdiv10</TT>
	<TD>The real stream obtained by dividing every element from stream #2
	  by ten.
    </TABLE>
  </CENTER>
  <P>Note that a (lazy) stream is a list (with elements separated
    by <TT>::</TT>) terminated by a future.</P>
  <P>The other window, entitled&nbsp;<TT>E</TT>, is the entry window.
    Always enter two space-seperated integers (stream number, value) at
    a time, followed by ENTER.  To enter the value 100 on input stream #1,
    enter:</P>
  <PRE>
	1 100</PRE>
  <P>To then enter the value 101 on stream #2, enter:</P>
  <PRE>
	2 101</PRE>
  <P>The other streams will immediately fill up when sufficient input
    is available.</P>


<?php section("smolka", "futures et al.") ?>

  <P>
    Here are some small examples demonstrating concurrent programming with
    futures in Alice:
  </P>

  <UL>
    <LI> <A href="../examples/smolka.aml"><TT>smolka.aml</TT></A> </LI>
  </UL>


<?php section("more", "more") ?>

  <P>
    You will find some more examples in:
  </P>

  <UL>
    <LI> <A href="../examples/"><TT>../examples/</TT></A> </LI>
  </UL>


<?php footing() ?>
