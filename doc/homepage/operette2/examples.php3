<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 2 - Examples", "examples") ?>



<?php section("streams", "streams") ?>

  <P>
    This program allows the user to interactively put numbers on two streams.
    Several other streams are concurrently produced by different combinations
    of those two streams (addition, zipping, mapping, etc.). All streams are
    displayed in the Inspector.
  </P>

  <UL>
    <LI> <A href="examples/streams.aml"><TT>streams.aml</TT></A> </LI>
  </UL>


<?php section("smolka", "futures et al.") ?>

  <P>
    Here are some small examples for concurrent programming with futures:
  </P>

  <UL>
    <LI> <A href="examples/smolka.aml"><TT>smolka.aml</TT></A> </LI>
  </UL>


<?php section("interpreter", "interpreter") ?>

  <P>
    The following interpreter for a simple functional language adapted from
    last semester's course "Programmierung" demonstrates
    how a program can be divided into components:
  </P>

  <UL>
    <LI> <A href="examples/interpreter/">Interpreter for F</A> </LI>
    <LI> Everything as a <A href="examples/interpreter.tar"><TT>.tar</TT></A>
         file </LI>
  </UL>

  <P>
    Compile it with
  </P>

  <PRE>
	stoc -x Main.aml -o interpr
  </PRE>


<?php footing() ?>
