<?php include("macros.php3"); ?>

<?php heading("Oz for Lightning - Examples", "examples") ?>



<?php section("streamadd", "stream adder") ?>

  <P>
    This program does pairwise addition of the elements of two integer streams,
    producing a third stream. The user can interactively put numbers on both
    of the two source streams. All three streams are displayed in the
    Inspector.
  </P>

  <UL>
    <LI> <A href="examples/streamadd.aml"><TT>streamadd.aml</TT></A> </LI>
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
