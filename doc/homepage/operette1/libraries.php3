<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">

<HTML>
  <HEAD>
    <TITLE>Stockhausen Operette 1 - Threads</TITLE>
    <LINK rel="stylesheet" type="text/css" href="style.css">
  </HEAD>

  <BODY>

  <H1>
  stock<BR>
  hausen.<BR>
  <BR>
  -<BR>
  libraries<BR>
  -
  </H1>

  <?php
    include ("menu.php3")
  ?>

  <H2>overview ___________________________</H2>
  <BR><BR>

  <P>
    Stockhausen is equipped with <A href="#basis">parts</A> of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/sml-std-basis.html">Standard
    ML Basis Library</A>. In addition, we currently provide two structures
    dealing with
  </P>

  <UL>
    <LI> <A href="transients.php3">transients</A>
         (<TT>structure <A href="transients.php3#sig">Transient</A></TT>),
	 and </LI>
    <LI> <A href="threads.php3">threads</A>
         (<TT>structure <A href="threads.php3#sig">Thread</A></TT>). </LI>
  </UL>


  <H2><A name=basis>top-level __________________________</A></H2>
  <BR><BR>

  <P>
    The Standard ML
    <A href="http://www.dina.kvl.dk/~sestoft/sml/top-level-chapter.html">
    top-level environment</A> has been implemented almost completely, except
    for several functions dealing with the <TT>real</TT> type and the
    special purpose procedure <TT>use</TT>.
  </P>

  <P>
    Note however that Stockhausen does not currently support overloading,
    so all overloaded operations in the top-level are only available at their
    default type.
  </P>


  <H2><A name=basis>basis library ______________________</A></H2>
  <BR><BR>

  <P>
    Stockhausen Operette 1 only provides parts of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/sml-std-basis.html">SML
    Basis Library</A>. The following structures are complete:
  </P>

  <UL>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/general.html">General</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/option.html">Option</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/list.html">List</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/list-pair.html">ListPair</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/string-cvt.html">StringCvt</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/command-line.html">CommandLine</A>
      </TT>
      (import from <TT>"x-alice:/lib/CommandLine.ozf"</TT>)
    </LI>
  </UL>

  <P>
    Other structures are only partially implemented:
  </P>

  <UL>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/bool.html">Bool</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/char.html">Char</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/string.html">String</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/integer.html">Int</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/integer.html">LargeInt</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/word.html">Word</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/word.html">LargeWord</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/real.html">Real</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/real.html">LargeReal</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/array.html">Array</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/vector.html">Vector</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/io.html">IO</A>
      </TT>
      (import from <TT>"x-alice:/lib/IO.ozf"</TT>)
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/text-io.html">TextIO</A>
      </TT>
      (import from <TT>"x-alice:/lib/TextIO.ozf"</TT>)
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/os-process.html">OS.Process</A>
      </TT>
      (import <TT>OS</TT> from <TT>"x-alice:/lib/OS.ozf"</TT>)
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/unix.html">Unix</A>
      </TT>
      (import from <TT>"x-alice:/lib/Unix.ozf"</TT>)
    </LI>
  </UL>

  <P>
    Some of the above structures must be imported explicitly. A program using
    the <TT>IO</TT> structure, for example, has to be
    prefixed by the following import announcement
    (see <A href="components.php3">components</A>):
  </P>

  <PRE>
	import structure IO from "x-alice:/lib/IO.ozf"
  </PRE>


  <BR>
  <HR>
  <DIV ALIGN=RIGHT>
    <ADDRESS>
       <A href="/~rossberg/">Andreas Rossberg</A> -
       last modified <?php echo date("Y/m/d") ?>
    </ADDRESS>
  </DIV>

  </BODY>
</HTML>
