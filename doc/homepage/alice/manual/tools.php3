<?php include("macros.php3"); ?>

<?php heading("Tools", "tool\nsupport") ?>

<?php section("overview", "overview") ?>

<P>The Alice system features the following command line tools:</P>

<UL>
  <LI><A href="interactive.php3"><TT>alice</TT></A>, the interactive top-level </LI>
  <LI><A href="compiler.php3"><TT>alicec</TT></A>, the batch compiler </LI>
  <LI><A href="machine.php3"><TT>alicerun</TT></A>, the virtual machine </LI>
  <LI><A href="linker.php3"><TT>alicelink</TT></A>, the static linker </LI>
  <LI><A href="depend.php3"><TT>alicedep</TT></A>, the dependency analyzer </LI>
</UL>

<P>In addition, the following interactive browsers are available as part
of the library:</P>

<UL>
  <LI><A href="library/inspector.php3"><EM>Inspector</EM></A>, for browsing data
  structures </LI>
  <LI><A href="library/explorer.php3"><EM>Explorer</EM></A>, for browsing search
  trees </LI>
</UL>

<P>Also note that Alice can <A href="interop.php3">interoperate</A> with <A
href="http://www.mozart-oz.org/">Mozart</A>, so that most Mozart tools can be
used from within Alice.</P>


<?php footing() ?>
