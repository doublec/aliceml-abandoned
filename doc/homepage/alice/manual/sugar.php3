<?php include("macros.php3"); ?>

<?php heading("Syntactic enhancements", "Syntactic enhancements") ?>


<?php section("overview", "overview") ?>

<P>The extensions listed here are mostly syntactic sugar that is also
expressible by other, less convenient means:</P>

<UL>
  <LI> <A href="sugar.php3#lexical">lexical syntax</A> </LI>
  <LI> <A href="sugar.php3#longids">long identifiers</A> </LI>
  <LI> <A href="sugar.php3#vectors">vector expressions and patterns</A> </LI>
  <LI> <A href="sugar.php3#records">records</A> </LI>
  <LI> <A href="sugar.php3#patterns">pattern language</A> </LI>
  <LI> <A href="sugar.php3#rec">recursive definitions</A> </LI>
</UL>


<?php section("lexical", "lexical syntax extensions") ?>

<?php subsection("literals", "Literals") ?>

<P>Numeric literals may contain underscores to group digits:</P>

<PRE>
	val pi = 3.141_592_653_596
	val billion = 1_000_000_000
	val nibbles = 0wx_f300_4588</PRE>

<P>Moreover, binary integer and word literals are supported:</P>

<PRE>
	val ten  = 0b1010
	val bits = 0wb1101_0010_1111_0010</PRE>


<?php subsection("longids", "Long identifiers") ?>

<P>Long identifiers are not part of the lexical syntax, but of the context-free
grammar. Consequently, there may be arbitrary white space separating the dots
from the identifiers:</P>

<PRE>
	mod . submod (*Here!*) . subsub
	    . f</PRE>


<?php section("vectors", "vectors") ?>

<P>Following SML/NJ, Alice provides vector expressions and patterns:</P>

<PRE>
	val v = #[1, 2, 4, 1, 2]

	fun f #[]  = 0
	  | f #[n] = n
	  | f  v   = 1</PRE>

<P>The syntax is:</P>

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
</TABLE>
  


<?php section("records", "records") ?>

<P>Two extensions improve handling of records:</P>

<UL>
  <LI> Punning </LI>
  <LI> Functional update </LI>
</UL>


<?php subsection("records-punning", "Record punning") ?>

<P>The first extension is a simple derived form for record expressions to allow
punning:</P>

<PRE>
	fun f {a,b,c} = {a,b}</PRE>

<P>is understood as an abbreviation for</P>

<PRE>
	fun f {a = a, b = b, c = c} = {a = a, b = b}</PRE>

<P>While SML allows punning in record patterns (so that the left hand side of
the former example is legal), it does not allow punning in record expressions.
In Alice the latter is also available as a simple derived form (dualing the
derived form for patterns):</P>

<TABLE>
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
    <TD> </TD>
  </TR>
</TABLE>

<P>For example, the expression <TT>{a, b : int, c}</TT> abbreviates
<TT>{a = a, b = b : int, c = c}</TT>.</P>


<?php subsection("records-update", "Functional record update") ?>

<P>The second extension is a syntax for functional record update. For
example,</P>

<PRE>
	let
	    val r = {a = 1, b = true, c = "hello"}
	in
	    {r where a = 3, c = "bye"}
	end</PRE>

<P>evaluates to</P>

<PRE>
	{a = 3, b = true, c = "bye"}</PRE>

<P>More precisely, the following syntax is supported:</P>

<TABLE>
  <TR>
    <TD> <I>exp</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>{</TT> <I>atexp</I> <TT>where</TT> <I>exprow</I> <TT>}</TT> </TD>
    <TD> record update </TD>
  </TR>
</TABLE>

<P>The expression <I>atexp</I> must have a record type that includes all fields
contained in <I>exprow</I>. The types of the fields must match. The result of
evaluating a record update is a record of the same type but with the fields
occuring in <I>exprow</I> replacing the corresponding values of the original
record.</P>


<?php section("patterns", "patterns") ?>

<P>
    The language provides several useful additions to SML's patterns:
  </P>

<TABLE>
  <TR>
    <TD> <I>atpat</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR valign=baseline>
    <TD></TD> <TD></TD>
    <TD> <TT>(</TT> <I>pat</I><SUB>1</SUB> <TT>|</TT> ... <TT>|</TT>
                    <I>pat</I><SUB><I>n</I></SUB> <TT>)</TT> </TD>
    <TD> alternative (<I>n</I>&ge;2) </TD>
  </TR>
  <TR></TR>
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

  <P>
    Alternative patterns (also called <I>or patterns</I>) are present in SML/NJ
    as well and allow more compact case analysis:
  </P>

  <PRE>
	fun f(1 | 2 | 3) = 0
	  | f n          = n
  </PRE>

  <P>
    The patterns nested inside an alternative pattern may bind variables, but
    all patterns must bind exactly the same set of variables with the same type.
  </P>

  <P>
    Layered patterns (also called <I>as patterns</I>) have been generalized to
    allow arbitrary patterns on both sides (in contrast to just an identifier
    on the left hand side as in SML). This is useful as it allows to put the
    identifier on either side:
  </P>

  <PRE>
	fun f(xs as x::xr) = bla
	fun g(x::xr as xs) = blo
  </PRE>

  <P>
    Negated patterns are a more special feature. A negated pattern matches
    exactly when the operand pattern does not match. This sometimes allows
    specifying cases in a more natural order,
    in particular in combination with or patterns:
  </P>

  <PRE>
	fun f(non(1 | 2 | 3)) = 0
	  | f n               = n
  </PRE>

  <P>
    The nested pattern may bind variables, but these are not visible outside.
    (They may be useful for a local guard, for example.)
  </P>

  <P>
    The most important extension are pattern guards. These allow decorating
    patterns with boolean conditions. A guarded pattern matches, if the
    pattern matches and the guard expression evaluates to <TT>true</TT>.
    The guard expression can refer to variables bound by the nested pattern:
  </P>

  <PRE>
	fun f(x,y) where (x = y) = g x
	  | f(x,y)               = h y
  </PRE>



<?php section("rec", "recursive definitions") ?>

<?php subsection("rec-declaration", "Recursive declarations") ?>

<P>SML only allows function expressions on the right hand side of <TT>val</TT>
<TT>rec</TT>. Alice is a bit more permissive. For example, one can construct
cyclic lists:</P>

<PRE>
	val xs = 1::2::xs</PRE>

<P>Or regular trees:</P>

<PRE>
	datatype tree = LEAF | BRANCH of tree * tree

	val tree = BRANCH(BRANCH(tree,LEAF), tree)</PRE>

<P>The right-hand sides of recursive bindings may be any expressions that are
<EM>non-expansive</EM> (i.e. syntactic values) and match the corresponding
left-hand side patterns (statically). Unfounded recursion is legal, but
evaluates to <A href="futures.php3">futures</A> that cannot be eliminated:</P>

<PRE>
	val (x,y) = (y,x)</PRE>

<P>Note that the same data structures are constructable by explicit use of <A
href="futures.php#promises">promises</A>.</P>


<?php subsection("rec-expression", "Recursive expressions") ?>

<P>Recursive values may be constructed directly without resorting to recursive 
declarations:</P>

<PRE>
	val l = rec xs => 1::2::xs</PRE>

<P>The respective syntax is:</P>

<TABLE>
  <TR>
    <TD> <I>exp</I> </TD>
    <TD align="center">::=</TD>
    <TD> ... </TD>
    <TD> </TD>
  </TR>
  <TR>
    <TD></TD> <TD></TD>
    <TD> <TT>rec</TT> <I>pat</I> <TT>=></TT> <I>exp</I> </TD>
    <TD> recursion </TD>
  </TR>
</TABLE>

<P>Such <TT>rec</TT> expressions are expanded as a derived form:</P>

<TABLE>
  <TR>
    <TD> <TT>rec</TT> <I>pat</I> <TT>=></TT> <I>exp</I> </TD>
    <TD> &nbsp;&nbsp;==>&nbsp;&nbsp; </TD>
    <TD> <TT>let</TT> <TT>val</TT> <TT>rec</TT> <I>vid</I> <TT>as</TT> <I>pat</I>
    <TT>=</TT> <I>exp</I> <TT>in</TT> <I>vid</I> <TT>end</TT></TD>
  </TR>
</TABLE>

<P>where <I>vid</I> is a fresh identifier.</P>

<?php footing() ?>
