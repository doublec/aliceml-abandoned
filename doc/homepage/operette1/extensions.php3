<?php include("macros.php3"); ?>

<?php heading("Oz for Lightning - SML Core Language Extensions",
		"core <BR> language <BR> extensions") ?>



<?php section("overview", "overview") ?>

  <P>
    Besides <A href="modules.php3">extensions to the SML module system</A>,
    Alice also extends the SML core language with several features:
  </P>

  <UL>
    <LI> <A href="#data">structural datatypes</A> </LI>
    <LI> <A href="#opendata">open datatypes</A> </LI>
    <LI> <A href="#patterns">extended pattern language</A> </LI>
    <LI> <A href="#valrec">generalized <TT>val</TT> <TT>rec</TT></A> </LI>
    <LI> <A href="#vectors">vector expressions and patterns</A> </LI>
    <LI> <A href="#records">sugar for records</A> </LI>
  </UL>

  <P>
    Other important features of Alice, like
    <A href="laziness.php3">lazy evaluation</A>,
    <A href="futures.php3">futures</A>, and 
    <A href="threads.php3">threads</A>, show up as
    <A href="library.php3">library</A> primitives only.
  </P>



<?php section("data", "datatypes") ?>

  <P>
    In Alice datatypes are not generative. All datatypes that have
    structurally equivalent definitions are compatible. For example, the
    following program will elaborate:
  </P>

  <PRE>
	datatype 'a t = A | B of 'a | C of 'a t
	val x = C(B 0)

	datatype 'a u = B of 'a | C of 'a u | A
	val y = B 20

	datatype 'a v = B of 'a | C of 'a t | A
	val z = A

	val l = [x,y,z]
  </PRE>

  <P>
    This relaxation is particularly interesting for distributed programming.
    Structural datatypes correspond to atoms in Oz.
  </P>



<?php section("opendata", "open datatypes") ?>

  <P>
    Open datatypes are a generalization of SML's exception type. In effect,
    the programmer can arbitrarily introduce new sum types similar to
    <TT>exn</TT>, which have an potentially unlimited set of constructors.
  </P>

  <P>
    An open datatype is declared as follows:
  </P>

  <PRE>
	datatype 'a message

	constructor DoThis of int : 'a message
	constructor DoThat of bool * 'a : 'a message
	constructor StopIt : 'a message
	constructor Abort = StopIt
  </PRE>

  <P>
    Constructors can be added at any point. Like exceptions in SML, constructor
    declarations are dynamically generative, i.e. the following function
    returns a different constructor on each call:
  </P>

  <PRE>
	fun genMsg() = let constructor C : 'a message in C end
  </PRE>

  <P>
    Note that - like exceptions - open datatypes cannot admit equality (since
    it is unknown whether there will be any constructors for it prohibiting
    that).
  </P>

  <P>
    Constructors of open datatypes correspond to Oz names.
  </P>

  <P>
    Exception declarations and specifications can be turned into a derived form,
    e.g.
  </P>

  <PRE>
        exception Error of string
	  ==>
	constructor Error of string : exn
  </PRE>

  <P>
    Here is the complete syntax dealing with open datatypes:
  </P>

  <TABLE>
    <TR>
      <TD> <I>dec</I> </TD>
      <TD align="center">::=</TD>
      <TD> ... </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>constructor</TT> <I>dconbind</I> </TD>
      <TD> generative constructor </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>datbind</I> </TD>
      <TD align="center">::=</TD>
      <TD> ... </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>tyvarseq</I> <I>tycon</I> </TD>
      <TD> open datatype </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>dconbind</I> </TD>
      <TD align="center">::=</TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I> &lt;<TT>of</TT> <I>ty</I>&gt;
           <TT>:</TT> <I>tyvarseq</I> <I>longtycon</I>
	   &lt;<TT>and</TT> <I>dconbind</I>&gt; </TD>
      <TD> new constructor </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I>
           <TT>=</TT> &lt;<TT>op</TT>&gt; <I>longvid</I>
	   &lt;<TT>and</TT> <I>dconbind</I>&gt; </TD>
      <TD> synonym </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>spec</I> </TD>
      <TD align="center">::=</TD>
      <TD> ... </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>constructor</TT> <I>dcondesc</I> </TD>
      <TD> generative constructor </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>datdesc</I> </TD>
      <TD align="center">::=</TD>
      <TD> ... </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>tyvarseq</I> <I>tycon</I> </TD>
      <TD> open datatype </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>dcondesc</I> </TD>
      <TD align="center">::=</TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I> &lt;<TT>of</TT> <I>ty</I>&gt;
           <TT>:</TT> <I>tyvarseq</I> <I>longtycon</I>
	   &lt;<TT>and</TT> <I>dcondesc</I>&gt; </TD>
      <TD> new constructor </TD>
    </TR>
<!--
    <TR>
      <TD></TD> <TD></TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I>
           <TT>=</TT> &lt;<TT>op</TT>&gt; <I>longvid</I>
	   &lt;<TT>and</TT> <I>dcondesc</I>&gt; </TD>
      <TD> synonym </TD>
-->
    </TR>
  </TABLE>



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
                      <I>pat</I><SUB>n</SUB> <TT>)</TT> </TD>
      <TD> alternative (n>=2) </TD>
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



<?php section("valrec", "val rec") ?>

  <P>
    SML only allows function expressions on the right hand side of <TT>val</TT>
    <TT>rec</TT>. Alice is a bit more permissive. Roughly, it allows any
    right hand side which is a syntactic value.
  </P>

  <P>
    One can construct cyclic lists, for example:
  </P>

  <PRE>
	val xs = 1::2::xs
  </PRE>

  <P>
    Or rational trees:
  </P>

  <PRE>
	datatype tree = LEAF | BRANCH of tree * tree

	val tree = BRANCH(BRANCH(tree,LEAF), tree)
  </PRE>



<?php section("vectors", "vectors") ?>

  <P>
    Following SML/NJ, Alice provides vector expressions and patterns:
  </P>

  <PRE>
	val v = #[1, 2, 4, 1, 2]

	fun f #[]  = 0
	  | f #[n] = n
	  | f  v   = 1
  </PRE>

  <P>
    The syntax is obvious:
  </P>

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
                       <I>exp</I><SUB>n</SUB> <TT>]</TT> </TD>
      <TD> vector (n>=0) </TD>
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
                       <I>pat</I><SUB>n</SUB> <TT>]</TT> </TD>
      <TD> vector (n>=0) </TD>
    </TR>
  </TABLE>
  


<?php section("records", "records") ?>

  <P>
    There are two extensions for more comfortable handling of records.
    First is a simple derived form for record expressions to allow punning:
  </P>

  <PRE>
	fun f {a,b,c} = {a,b}
  </PRE>

  <P>
    While SML allows punning in record patterns (so that the left hand side of
    the example is legal), it does not allow punning in record expressions.
    Alice straightens this through a simple derived form (dualing
    the derived form for patterns):
  </P>

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

  <P>
    The second extension is a syntax for functional record update:
  </P>

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

  <P>
    The expression <I>atexp</I> must have a record type that includes all
    fields contained in <I>exprow</I>. The types of the fields must match.
    The result of evaluating a record update is a record of the same type but
    with some of the fields replaced according to <I>exprow</I>. For example,
  </P>

  <PRE>
	let
	    val r = {a = 1, b = true, c = "hello"}
	in
	    {r where a = 3, c = "bye"}
	end
  </PRE>

  <P>
    evaluates to
  </P>

  <PRE>
	{a = 3, b = true, c = "bye"}
  </PRE>


<?php footing() ?>
