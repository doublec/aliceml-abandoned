<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 2", "\"operette 2\"") ?>



<?php section("overview", "overview") ?>

  <P>
    We proudly present the second internal prototype release of the Stockhausen
    system. We call it <I>"Operette&nbsp;2"</I>.
  </P>

  <P>
    Stockhausen implements the language <I>Alice</I>, which integrates
    features from <A href="http://www.mozart-oz.org/">Oz/Mozart</A> with the
    language
    <A href="http://cm.bell-labs.com/cm/cs/what/smlnj/sml.html">Standard ML</A>.
    Moreover, Alice includes other interesting features found in neither of
    both languages.
  </P>

  <P>
    Stockhausen Operette 2 offers the following advances over
    <A href="../operette1/">Stockhausen Operette 1</A>:
  </P>

  <UL>
    <LI>runtime types
    <LI>pickling and type-safe unpickling
    <LI><A href="usage.php3#stot">an interactive toplevel</A>
    <LI>new libraries for constraint programming and
      building graphical user interfaces
    <LI>a new tool for dynamically inspecting Alice values
  </UL>

  <P>
    Just like Operette 1, besides implementing almost complete SML,
    Stockhausen Operette 2 continues to sport:
  </P>

  <UL>
    <LI> <A href="modules.php3"><I>higher-order modules</I></A>:
         a more powerful module language </LI>
    <LI> <A href="components.php3"><I>components</I></A>:
         type-safe dynamic loading of modules </LI>
    <LI> <A href="laziness.php3"><I>laziness</I></A>:
	 combining strict and lazy functional programming </LI>
    <LI> <A href="futures.php3"><I>futures</I></A>:
         "logic variables" and concurrency</LI>
  </UL>

  <P>
    Important design goals for Alice/Stockhausen were backward compatibility
    with Standard ML, as well as interoperability with Mozart.
    Stockhausen Operette 2 is based on the
    <A href="http://www.mozart-oz.org/">Mozart</A> virtual machine. Operette 2
    programs can therefore <A href="interop.php3">interoperate</A> with Oz.
  </P>

  <P align="CENTER"> <IMG src="../mozart_cartoon.jpg"> </P>



<?php section("people", "people") ?>

  <P>
    The following people are currently involved in the Alice/Stockhausen
    project:
  </P>

  <UL>
    <LI> <A href="/~bruni/">Thorsten Brunklaus</A> </LI>
    <LI> <A href="/~kornstae/">Leif Kornstaedt</A> </LI>
    <LI> <A href="/~rossberg/">Andreas Rossberg</A> </LI>
    <LI> <A href="/~smolka/">Gert Smolka</A> </LI>
  </UL>



<?php section("contact", "contact") ?>

  <P>
    Please send bug reports and other comments to:
  </P>

  <UL>
    <LI> <A href="mailto:stockhausen@ps.uni-sb.de" class=url>
         stockhausen@ps.uni-sb.de</A> </LI>
  </UL>

  <P>
    For discussion on Stockhausen and the language Alice see the
    local newsgroup:
  </P>

  <UL>
    <LI> <A href="news:ps.alice" class="url">ps.alice</A> </LI>
  </UL>



<?php section("download", "download") ?>

  <P>
    You can download versions of the Stockhausen logo:
  </P>

  <UL>
    <LI> <A href="../stockhausen.ps">Postscript</A> </LI>
    <LI> <A href="../stockhausen.jpg">JPEG</A> </LI>
  </UL>

  <P>
    Printed out on paper it will certainly prove to be a very useful item
    that is happy to stick to any wall available!
  </P>


<?php footing() ?>
