<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 1", "\"operette 1\"") ?>



<?php section("overview", "overview") ?>

  <P>
    We proudly present the first internal prototype release of the Stockhausen
    system. We call it <I>"Operette&nbsp;1"</I>.
  </P>

  <P>
    Stockhausen implements the language <I>Alice</I>, which integrates
    features from <A href="http://www.mozart-oz.org/">Mozart</A> with the
    language
    <A href="http://cm.bell-labs.com/cm/cs/what/smlnj/sml.html">Standard ML</A>.
    Moreover, Alice includes other interesting features found in neither of
    both languages.
    Besides implementing almost complete SML, Stockhausen Operette 1 already
    supports:
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
    Stockhausen Operette 1 is based on the
    <A href="http://www.mozart-oz.org/">Mozart</A> virtual machine. Operette 1
    programs can therefore <A href="interop.php3">interoperate</A> with Oz.
  </P>

  <P align="CENTER"> <IMG src="../mozart_cartoon.jpg"> </P>



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
    increasingly misnamed local newsgroup:
  </P>

  <UL>
    <LI> <A href="news:ps.amadeus" class="url">ps.amadeus</A> </LI>
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
