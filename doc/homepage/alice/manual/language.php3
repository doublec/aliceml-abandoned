<?php include("macros.php3"); ?>

<?php heading("The Language", "the\nlanguage") ?>


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
    <LI> <A href="futures.php3#spawn">concurrency</A> </LI>
    <LI> <A href="futures.php3#lazy">lazy evaluation</A> </LI>
    <LI> <A href="futures.php3#promise">promises</A> </LI>
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
    <LI> <A href="modules.php3#sigmembers">signature members and abstract signatures</A> </LI>
<!--
    <LI> <A href="modules.php3#paramsig">parameterized signatures</A> </LI>
    <LI> <A href="modules.php3#singleton">singleton signatures</A> </LI>
-->
    <LI> <A href="modules.php3#lazy">lazy evaluation</A> </LI>
    <LI> <A href="modules.php3#fixity">fixity specifications</A> </LI>
    <LI> <A href="modules.php3#sugar">syntax enhancements</A> </LI>
  </UL>
  </LI>

  <LI> <A href="packages.php3">Packages:</A>
  <UL>
    <LI> <A href="packages.php3#firstclass">first-class modules</A> </LI>
    <LI> <A href="packages.php3#dynamic">dynamic typing</A> </LI>
  </UL>
  </LI>

  <LI> <A href="pickling.php3">Pickling:</A>
  <UL>
    <LI> <A href="pickling.php3#semantics">persistent values</A> </LI>
    <LI> <A href="pickling.php3#export">exporting and importing modules</A> </LI>
  </UL>
  </LI>

  <LI> <A href="components.php3">Components:</A>
  <UL>
  </UL>
  </LI>

  <LI> <A href="distribution.php3">Distributed programming:</A>
  <UL>
  </UL>
  </LI>

  <LI> <A href="constraints.php3">Constraint programming</A> </LI>
  <UL>
    <LI> <A href="constraints.php3#fd">finite domain constraints</A> </LI>
    <LI> <A href="constraints.php3#fs">finite set contraints</A> </LI>
    <LI> <A href="constraints.php3#spaces">spaces</A> </LI>
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

<P>The following grammar collects all syntactic extensions of Alice ML
relative to Standard ML. Derived forms are marked (*).


<?php subsection("syntax-exps", "Expressions") ?>

<TABLE class=bnf>
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
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>{</TT> <I>atexp</I> <TT>where</TT> <I>exprow</I> <TT>}</TT> </TD>
    <TD> record update </TD>
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
    <TD> <TT>lazy</TT> <I>exp</I> </TD>
    <TD> lazy </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>spawn</TT> <I>exp</I> </TD>
    <TD> concurrent </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>rec</TT> <I>pat</I> <TT>=></TT> <I>exp</I> </TD>
    <TD> recursion (*) </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>pack</TT> <I>strexp</I> <TT>:></TT> <I>sigexp</I> </TD>
    <TD> packing </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>exprow</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <I>vid</I> &lt;<TT>:</TT> <I>ty</I>&gt;
                    &lt;<TT>,</TT> <I>exprow</I>&gt; </TD>
    <TD> label as expression (*) </TD>
  </TR>
</TABLE>


<?php subsection("syntax-pats", "Patterns") ?>

<TABLE class=bnf>
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
</TABLE>


<?php subsection("syntax-tys", "Types") ?>

<TABLE class=bnf>
  <TR>
    <TD> <I>ty</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>_</TT> </TD>
    <TD> wildcard </TD>
  </TR>
</TABLE>


<?php subsection("syntax-decs", "Declarations") ?>

<TABLE class=bnf>
  <TR>
    <TD> <I>dec</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> <SUP>1</SUP> </TD>
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
    <TD> <I>fvalbind</I> </TD>
    <TD align="center">::=</TD>
    <TD> &lt;<TT>lazy</TT> | <TT>spawn</TT>&gt; </TD>
    <TD> (<I>m</I>,<I>n</I>&ge;1) (*) </TD>
  </TR><TR>
    <TD></TD><TD></TD>
    <TD> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &lt;<TT>op</TT>&gt; <I>vid atpat</I><SUB>11</SUB> ... <I>atpat</I><SUB>1<I>n</I></SUB> &lt;<TT>:</TT> ty<SUB>1</SUB>&gt;
	 <TT>=</TT> <I>exp</I><SUB>1</SUB> </TD>
  </TR><TR>
    <TD></TD><TD></TD>
    <TD> &nbsp;&nbsp;&nbsp;&nbsp;<TT>|</TT>
         &lt;<TT>op</TT>&gt; <I>vid atpat</I><SUB>21</SUB> ... <I>atpat</I><SUB>2<I>n</I></SUB> &lt;<TT>:</TT> ty<SUB>2</SUB>&gt;
	 <TT>=</TT> <I>exp</I><SUB>2</SUB> </TD>
  </TR><TR>
    <TD></TD><TD></TD>
    <TD> &nbsp;&nbsp;&nbsp;&nbsp;<TT>|</TT> ... </TD>
  </TR><TR>
    <TD></TD><TD></TD>
    <TD> &nbsp;&nbsp;&nbsp;&nbsp;<TT>|</TT>
         &lt;<TT>op</TT>&gt; <I>vid atpat</I><SUB><I>m</I>1</SUB> ... <I>atpat</I><SUB><I>mn</I></SUB> &lt;<TT>:</TT> ty<SUB><I>m</I></SUB>&gt;
	 <TT>=</TT> <I>exp</I><SUB><I>m</I></SUB> </TD>
  </TR><TR>
    <TD></TD><TD></TD>
    <TD> &nbsp;&nbsp;&nbsp;&nbsp;&lt;<TT>and</TT> <I>fvalbind</I>&gt; </TD>
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
  <TR>
    <TD> <I>strbind</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>_</TT> &lt;<TT>:</TT> <I>sigexp</I>&gt; <TT>=</TT> <I>strexp</I>
         &lt;<TT>and</TT> <I>strbind</I>&gt; </TD>
    <TD> anonymous structure (*) </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>funbind</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>strid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB><I>n</I></SUB>
         <TT>=</TT> <I>strexp</I>
         &lt;<TT>and</TT> <I>funbind</I>&gt; </TD>
    <TD> functor binding (<I>n</I>&ge;1) (*) </TD>
  </TR>

<!--
  <TR></TR>
  <TR>
    <TD> <I>sigbind</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>sigid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB><I>n</I></SUB>
         <TT>=</TT> <I>sigexp</I>
         &lt;<TT>and</TT> <I>sigbind</I>&gt; </TD>
    <TD> signature binding (<I>n</I>&ge;0) </TD>
  </TR>
-->
</TABLE>

<P><SUP>1</SUP>) The extended phrase class <I>dec</I> contains all of Standard
ML's <I>dec</I>, <I>strdec</I> and <I>topdec</I>.</P>


<?php subsection("syntax-structs", "Structures") ?>

<TABLE class=bnf>
  <TR>
    <TD> <I>strexp</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>(</TT> <I>strexp</I> <TT>)</TT> </TD>
    <TD> parentheses </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>(</TT> <I>dec</I> <TT>)</TT> </TD>
    <TD> structure (*) </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <I>strexp</I> <I>strexp</I> </TD>
    <TD> functor application <SUP>1</SUP> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>unpack</TT> <I>infexp</I> <TT>:</TT> <I>sigexp</I> </TD>
    <TD> unpacking </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>fct</TT> <I>strpat</I> <TT>=></TT> <I>strexp</I> </TD>
    <TD> functor </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>strpat</I> </TD>
    <TD align="center">::=</TD>
    <TD> <TT>(</TT> <I>strid</I> <TT>:</TT> <I>sigexp</I> <TT>)</TT> </TD>
    <TD> parameter </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>(</TT> <TT>_</TT> <TT>:</TT> <I>sigexp</I> <TT>)</TT> </TD>
    <TD> anonymous parameter (*) </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>(</TT> <I>spec</I> <TT>)</TT> </TD>
    <TD> signature as parameter (*) </TD>
  </TR>
</TABLE>

<P><SUP>1</SUP>) See the <A href="modules.php3#syntax-structs">module syntax
summary</A> for a more precise grammar of structure expressions and functor
application.</P>


<?php subsection("syntax-sigs", "Signatures") ?>

<TABLE class=bnf>
  <TR>
    <TD> <I>sigexp</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>any</TT> </TD>
    <TD> top </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <I>longsigid</I> </TD>
    <TD> signature identifier </TD>
  </TR>
<!--
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>let</TT> <I>dec</I> <TT>in</TT> <I>sigexp</I> <TT>end</TT> </TD>
    <TD> local declarations </TD>
  </TR>
-->
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>(</TT> <I>sigexp</I> <TT>)</TT> </TD>
    <TD> parentheses </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>(</TT> <I>spec</I> <TT>)</TT> </TD>
    <TD> signature (*) </TD>
  </TR>
  <TR></TR>
<!--
  <TR>
    <TD> <I>appsigexp</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>atsigexp</I> </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <I>appsigexp</I> <I>atstrexp</I> </TD>
    <TD> signature application </TD>
  </TR>
  <TR></TR>
  <TR>
    <TD> <I>sigexp</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>appsigexp</I> </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD> <I>sigexp</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>atsigexp</I> </TD>
    <TD> </TD>
  </TR>
-->
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>fct</TT> <I>strpat</I> <TT>-></TT> <I>sigexp</I> </TD>
    <TD> functor </TD>
  </TR>
<!--
  <TR>
    <TD></TD> <TD></TD>
    <TD> <I>sigexp</I> <TT>where</TT> <I>rea</I> </TD>
    <TD> specialization </TD>
  </TR>
  <TR></TR>
  <TR>
    <TD> <I>rea</I> </TD>
    <TD align="center">::=</TD>
    <TD> <TT>val</TT> <I>longvid</I> <TT>=</TT> <I>longvid</I> </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>constructor</TT> <I>longvid</I> <TT>=</TT> <I>longvid</I> </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>type</TT> <I>tyvarseq</I> <I>longtycon</I>
         <TT>=</TT> <I>ty</I> </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>structure</TT> <I>longstrid</I><SUB>1</SUB> <TT>=</TT> <I>longstrid</I><SUB>2</SUB> </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>signature</TT> <I>longsigid</I>
         <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB><I>n</I></SUB>
         <TT>=</TT> <I>sigexp</I> </TD>
    <TD> signature (<I>n</I>&ge;0) </TD>
  </TR>
  <TR></TR>
-->
</TABLE>


<?php subsection("syntax-specs", "Specifications") ?>

<TABLE class=bnf>
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
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>functor</TT> <I>fundesc</I> </TD>
    <TD> functor specification (*) </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>signature</TT> <I>sigdesc</I> </TD>
    <TD> signature specification </TD>
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

  <TR></TR>
  <TR>
    <TD> <I>fundesc</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>strid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB><I>n</I></SUB>
         <TT>:</TT> <I>sigexp</I>
         &lt;<TT>and</TT> <I>fundesc</I>&gt; </TD>
    <TD> functor description (<I>n</I>&ge;1) (*) </TD>
  </TR>

  <TR></TR>
  <TR>
    <TD> <I>sigdesc</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>sigid</I> &lt;<TT>=</TT> <I>sigexp</I>&gt;
         &lt;<TT>and</TT> <I>sigdesc</I>&gt; </TD>
    <TD> signature description </TD>
  </TR>
<!--
  <TR>
    <TD> <I>sigdesc</I> </TD>
    <TD align="center">::=</TD>
    <TD> <I>sigid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB><I>n</I></SUB>
         &lt;<TT>=</TT> <I>sigexp</I>&gt;
         &lt;<TT>and</TT> <I>sigdesc</I>&gt; </TD>
    <TD> signature description (<I>n</I>&ge;0) </TD>
  </TR>
-->
</TABLE>

<?php footing() ?>
