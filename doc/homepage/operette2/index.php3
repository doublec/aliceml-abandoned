<?php include("macros.php3"); ?>

<?php heading("Oz for Lightning", "\"Alice\"") ?>



<?php section("overview", "overview") ?>

  <P>
    Our Lightning prototype implements a statically typed dialect of
    the language <A href="http://www.mozart-oz.org/">Oz</A> that we named
    <I>Alice</I>.
  </P>

  <P>
    Alice integrates
    features from <A href="http://www.mozart-oz.org/">Oz</A> with
    <A href="http://cm.bell-labs.com/cm/cs/what/smlnj/sml.html">Standard ML</A>.
    Moreover, Alice includes other interesting features found in neither of
    both languages.
    Besides implementing almost complete SML, Alice already supports:
  </P>

  <UL>
    <LI> <A href="laziness.php3"><I>laziness</I></A>:
	 combining strict and lazy functional programming </LI>
    <LI> <A href="futures.php3"><I>futures</I></A>:
         "logic variables" and concurrency</LI>
    <LI> <A href="components.php3"><I>components</I></A>:
         type-safe dynamic loading of modules </LI>
    <LI> <A href="modules.php3"><I>higher-order modules</I></A>:
         a more powerful module language </LI>
  </UL>



<?php section("people", "people") ?>

  <P>
    The following people are currently involved in the Alice project:
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

<!--
  <P>
    For discussion on Stockhausen and the language Alice see the
    local newsgroup:
  </P>

  <UL>
    <LI> <A href="news:ps.alice" class="url">ps.alice</A> </LI>
  </UL>
-->


<?php footing() ?>
