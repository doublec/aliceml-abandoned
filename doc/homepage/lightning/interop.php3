<?php include("macros.php3"); ?>

<?php heading("Oz for Lightning - Interoperability",
	      "interoper<BR>ability") ?>

<?php section("overview", "overview") ?>
  <P>This document explains
    how Alice data structures are mapped
    to Lightning data structures.</P>

<?php section("datarepresentation", "data representation") ?>

  <H3>Basic Types</H3>
  <P>Values of basic types are mapped to corresponding Lightning types:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Type<TH>Lightning Representation
      <TR><TD>int<TD><TT>System.Int32</TT>
      <TR><TD>char<TD><TT>System.Char</TT>
      <TR><TD>word<TD><TT>System.Int32</TT>
      <TR><TD>real<TD><TT>System.Float64</TT>
      <TR><TD>string<TD><TT>System.String</TT>
    </TABLE>
  </CENTER>

  <H3>Tuples and Records</H3>
  <P>Alice records are translated to arrays. Record fields are
    sorted according to the lexicographic ordering of their labels.
    The empty record is treated specially. Note that tuples are just
    special cases of records with numeric labels.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Type<TH>Lightning Representation
      <TR>
	<TD>Unit value <TT>()</TT> resp. <TT>{}</TT>
	<TD><TT>Int32(0)</TT>
      <TR><TD>Record<TD><TT>System.Array</TT>
    </TABLE>
  </CENTER>

  <H3>Procedures</H3>
  <P>Conceptually, all Alice functions are unary.  The typical idiom is
    to pass multiple arguments as a tuple of arguments.  This case is
    treated specially by the Alice compiler.</P>

  <P><I>(Section under construction)</I></P>


  <H3>Constructors</H3>

  <P><I>(Section under construction)</I></P>

  <H3>Constructed Values</H3>
  <P>Conceptually, all Alice constructors are unary.  For interoperability
    and efficiency,
    constructors syntactically declared taking a record as argument are
    treated as <A href="incompatibilities.php3#conarity"><I>n</I>-ary
    constructors</A>.</P>

  <P><I>(Section under construction)</I></P>


  <H3>Futures</H3>

  <P><I>(Section under construction)</I></P>


  <H3>Other Library Types</H3>
  <P>Some abstract library types are implemented natively:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Library Type<TH>Lightning Representation
      <TR><TD><TT>Array.array</TT><TD><TT>System.Array</TT>
      <TR><TD><TT>Vector.vector</TT><TD><TT>System.Array</TT>
      <TR><TD><TT>Thread.thread</TT><TD><TT>System.Threading.Thread</TT>
      <TR><TD><TT>TextIO.instream</TT><TD><TT>System.IO.TextReader</TT>
      <TR><TD><TT>TextIO.outstream</TT><TD><TT>System.IO.TextWriter</TT>
    </TABLE>
  </CENTER>


  <H3>Modules</H3>
  <P>Alice modules are translated to Lightning data structures such:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Module<TH>Lightning Representation
      <TR><TD>Structure<TD>like records
      <TR><TD>Functor<TD>like functions
    </TABLE>
  </CENTER>
  <P>Structure members are sorted lexicographically in two groups:
    first all module members (structures and functors), then all value
    members (values and constructors).
    Signatures and types are not yet represented at run-time.  Note that
    signature coercions can change structure representations.</P>


  <H3>Components</H3>
  <P>Alice components are DLL's:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Component<TH>Lightning Representation
      <TR><TD>Compiled component<TD>DLL with class <TT>Execute</TT>
    </TABLE>
  </CENTER>
  <P>To be used as a component a DLL must export a class <TT>Execute</TT>
    with a method</P>
  <PRE>
	public static Object Main(Object Composer)
  </PRE>
  <P>The argument to <TT>Main</TT> is a handle for the composer (to resolve
    imports). The result of the method must be the representation of the
    components body, which is considered to be a single structure.</P>
  <P>Components not written in Alice can be combined with Alice
    components by <A href="usage.php3#foreign">providing suitable signatures</A>
    at compile time.</P>

<?php footing() ?>
