<?php include("macros.php3"); ?>

<?php heading("The Language", "The Language") ?>


<?php section("overview", "overview") ?>

<P>Alice ML is based on the
<A href="http://cm.bell-labs.com/cm/cs/what/smlnj/sml.html">Standard ML</A>
programming language (SML), as defined in</P>

<UL>
  <LI>Robin Milner, Mads Tofte, Robert Harper, Dave MacQueen: <BR>
  <I><A href="http://mitpress.mit.edu/book-home.tcl?isbn=0-262-63181-4">The
  Definition of Standard ML (Revised)</A></I> <BR>
  The MIT Press <BR>
  ISBN 0-262-63181-4</LI>
</UL>

<P>However, Alice ML features several major extensions relative to SML:</P>

<UL>
  <LI> <A href="futures.php3">Futures:</A>
  <UL>
    <LI> <A href="futures.php3#lazy">lazy evaluation</A> </LI>
    <LI> <A href="futures.php3#spawn">concurrency</A> </LI>
    <LI> <A href="futures.php3#promises">promises</A> </LI>
  </UL>
  </LI>

  <LI> <A href="types.php3">Extended type language:</A>
  <UL>
    <LI> <A href="types.php3#datatype">structural datatypes</A> </LI>
    <LI> <A href="types.php3#exttype">extensible types</A> </LI>
    <LI> <A href="types.php3#wildcard">type wildcards</A> </LI>
  </UL>
  </LI>

  <LI> <A href="modules.php3">Higher-order modules:</A>
  <UL>
    <LI> <A href="modules.php3#local">local modules</A> </LI>
    <LI> <A href="modules.php3#higher">higher-order functors</A> </LI>
    <LI> <A href="modules.php3#sigmembers">signature members</A> </LI>
<!--
    <LI> <A href="modules.php3#paramsig">parameterized signatures</A> </LI>
    <LI> <A href="modules.php3#singleton">singleton signatures</A> </LI>
-->
    <LI> <A href="modules.php3#fixity">fixity specifications</A> </LI>
    <LI> <A href="modules.php3#wildcards">wildcard bindings</A> </LI>
    <LI> <A href="modules.php3#lazy">lazy evaluation</A> </LI>
  </UL>
  </LI>

  <LI> <A href="dynamics.php3">Dynamic typing:</A>
  <UL>
    <LI> <A href="dynamics.php3#packages">packages</A> </LI>
    <LI> <A href="dynamics.php3#components">components</A> </LI>
  </UL>
  </LI>

  <LI> <A href="distribution.php3">Distributed programming:</A>
  <UL>
    <LI> <A href="distribution.php3#pickling">pickling</A> </LI>
    <LI> <A href="distribution.php3#remote">distribution</A> </LI>
  </UL>
  </LI>

  <LI> <A href="constraints.php3">Constraint programming</A> </LI>
  <UL>
    <LI> <A href="constraints.php3#fd">finite domain constraints</A> </LI>
    <LI> <A href="constraints.php3#fs">finite set contraints</A> </LI>
    <LI> <A href="constraints.php3#search">programmable search</A> </LI>
  </UL>
  </LI>

  <LI> <A href="sugar.php3">Syntactic sugar:</A>
  <UL>
    <LI> <A href="sugar.php3#literals">extended syntax for numeric literals</A> </LI>
    <LI> <A href="sugar.php3#vectors">vector expressions and patterns</A> </LI>
    <LI> <A href="sugar.php3#records">sugar for records</A> </LI>
    <LI> <A href="sugar.php3#patterns">extended pattern language</A> </LI>
    <LI> <A href="sugar.php3#rec">generalized recursive definitions</A> </LI>
  </UL>
  </LI>
</UL>

<P>Note that Alice ML also has some minor
<A href="incompatibilities.php3">incompatibilities</A> with SML.</P>


<?php section("syntax", "syntax summary") ?>

<?php subsection("reserved", "Reserved words") ?>

<P>Alice defines some additional identifiers as reserved words:</P>

<PRE>
	any    constructor   exttype    fct      from       import     non
	lazy   pack          spawn      unpack   withfun    withval
</PRE>


<?php subsection("grammar", "Grammar") ?>

<P>The following grammar collects all syntactic extensions of Alice ML
relative to Standard ML. Derived forms are marked (*).

<TABLE>
  <TR>
    <TD> <I>atexp</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR valign=baseline>
    <TD></TD> <TD></TD>
    <TD> <TT>#[</TT> <I>exp</I><SUB>1</SUB> <TT>,</TT> ... <TT>,</TT>
                     <I>exp</I><SUB><I>n</I></SUB> <TT>]</TT> </TD>
    <TD> vector (<I>n</I>&ge;0) </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>exp</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>rec</TT> <I>pat</I> <TT>=></TT> <I>exp</I> </TD>
    <TD> recursion (*) </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>{</TT> <I>atexp</I> <TT>where</TT> <I>exprow</I> <TT>}</TT> </TD>
    <TD> record update </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>exprow</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> label as expression (*) </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <I>vid</I> &lt;<TT>:</TT> <I>ty</I>&gt;
                    &lt;<TT>,</TT> <I>exprow</I>&gt; </TD>
    <TD> </TD>
  </TR>

  <TR></TR>
  <TR></TR>

  <TR>
    <TD> <I>atpat</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR valign=baseline>
    <TD></TD> <TD></TD>
    <TD> <TT>#[</TT> <I>pat</I><SUB>1</SUB> <TT>,</TT> ... <TT>,</TT>
                     <I>pat</I><SUB><I>n</I></SUB> <TT>]</TT> </TD>
    <TD> vector (<I>n</I>&ge;0) </TD>
  </TR>
  <TR valign=baseline>
    <TD></TD> <TD></TD>
    <TD> <TT>(</TT> <I>pat</I><SUB>1</SUB> <TT>|</TT> ... <TT>|</TT>
                    <I>pat</I><SUB><I>n</I></SUB> <TT>)</TT> </TD>
    <TD> alternative (<I>n</I>&ge;2) </TD>
  </TR>

  <TR>&nbsp;</TR>
  <TR>
    <TD> <I>pat</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <I>pat</I> <TT>as</TT> <I>pat</I> </TD>
    <TD> layered (R) </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>non</TT> <I>pat</I> </TD>
    <TD> negated </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <I>pat</I> <TT>where</TT> <I>atexp</I> </TD>
    <TD> guarded (L) </TD>
  </TR>

  <TR></TR>
  <TR></TR>

  <TR>
    <TD> <I>ty</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD width="100"> <TT>_</TT> </TD>
    <TD> wildcard </TD>
  </TR>

  <TR></TR>
  <TR></TR>

  <TR>
    <TD> <I>dec</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>exttype</TT> <I>extbind</I> </TD>
    <TD> extensible datatype </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>constructor</TT> <I>econbind</I> </TD>
    <TD> generative constructor </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>extbind</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>tyvarseq</I> <I>tycon</I> </TD>
    <TD> extensible datatype </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>econbind</I> </TD>
    <TD align="center">::=</TD>
    <TD> &lt;<TT>op</TT>&gt; <I>vid</I> &lt;<TT>of</TT> <I>ty</I>&gt;
         <TT>:</TT> <I>tyvarseq</I> <I>longtycon</I>
	 &lt;<TT>and</TT> <I>econbind</I>&gt; </TD>
    <TD> new constructor </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> &lt;<TT>op</TT>&gt; <I>vid</I>
         <TT>=</TT> &lt;<TT>op</TT>&gt; <I>longvid</I>
	 &lt;<TT>and</TT> <I>econbind</I>&gt; </TD>
    <TD> synonym </TD>
  </TR>

  <TR></TR>
  <TR></TR>

  <TR>
    <TD> <I>spec</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>exttype</TT> <I>extdesc</I> </TD>
    <TD> extensible datatype </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>constructor</TT> <I>econdesc</I> </TD>
    <TD> generative constructor </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>extdesc</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>tyvarseq</I> <I>tycon</I> </TD>
    <TD> extensible datatype </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>econdesc</I> </TD>
    <TD align="center">::=</TD>
    <TD> &lt;<TT>op</TT>&gt; <I>vid</I> &lt;<TT>of</TT> <I>ty</I>&gt;
         <TT>:</TT> <I>tyvarseq</I> <I>longtycon</I>
	 &lt;<TT>and</TT> <I>econdesc</I>&gt; </TD>
    <TD> new constructor </TD>
  </TR>
<!--
  <TR>
    <TD></TD> <TD></TD>
    <TD> &lt;<TT>op</TT>&gt; <I>vid</I>
         <TT>=</TT> &lt;<TT>op</TT>&gt; <I>longvid</I>
	 &lt;<TT>and</TT> <I>econdesc</I>&gt; </TD>
    <TD> synonym </TD>
-->
  </TR>
</TABLE>

<?php footing() ?>
