<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">

<HTML>
  <HEAD>
    <TITLE>Stockhausen Operette 1</TITLE>
    <LINK rel="stylesheet" type="text/css" href="style.css">
  </HEAD>

  <BODY>

  <H1>
  stock<BR>
  hausen.<BR>
  <BR>
  "operette 1"
  </H1>

  <?php
    include ("menu.php3")
  ?>


  <H2>overview _____________________</H2>
  <BR><BR>

  <P>
    We proudly present the first internal prototype release of the Stockhausen
    system. We call it
  </P>

  <P class=quote>
    "Operette&nbsp;1".
  </P>

  <P>
    Stockhausen is based on 
    <A href="http://cm.bell-labs.com/cm/cs/what/smlnj/sml.html">Standard ML</A>
    but adds many of the interesting features from
    <A href="http://www.mozart-oz.org/">Mozart</A>. Operette 1 already supports:
  </P>

  <UL>
    <LI> logic variables and lazy computations
         (<A href="transients.php3"><I>transients</I></A>) </LI>
    <LI> concurrency and distribution
         (<A href="threads.php3"><I>threads</I></A>) </LI>
    <LI> type-safe dynamic loading of modules
         (<A href="components.php3"><I>components</I></A>) </LI>
  </UL>

  <P>
    Moreover, Stockhausen adds other interesting features neither found in
    Mozart nor in SML, like typed <A href="modules.php3">higher-order
    modules</A> and <A href="extensions.php3"">other extensions</A>.
  </P>

  <P>
    Stockhausen Operette 1 is based on the
    <A href="http://www.mozart-oz.org/">Mozart</A> engine. Operette 1 programs
    can therefore <A href="interop.php3">interoperate</A> with Oz.
  </P>

  <P align="CENTER"> <IMG src="../mozart_cartoon.jpg"> </P>


  <H2><A name="contact">contact ______________________</A></H2>
  <BR><BR>

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


  <H2><A name="download">download _____________________</A></H2>
  <BR><BR>

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


  <BR>
  <HR>
  <DIV ALIGN=RIGHT>
    <ADDRESS>
       <A href="/~kornstae/">Leif Kornstaedt</A>,
       <A href="/~rossberg/">Andreas Rossberg</A> -
       last modified <?php echo date("Y/m/d") ?>
    </ADDRESS>
  </DIV>

  </BODY>
</HTML>
