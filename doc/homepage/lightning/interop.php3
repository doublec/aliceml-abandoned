<?php include("macros.php3"); ?>

<?php heading("Oz for NGWS - Interoperability",
	      "interoper<BR>ability") ?>

<?php section("overview", "overview") ?>
  <P>This document explains
    how Alice data structures are mapped
    to NGWS data structures.</P>

<?php section("datarepresentation", "data representation") ?>

  <P>Since Alice is a polymorphic language, values of different types need
    to be handled uniformly in parts of the generated code.  This requires
    us to choose a common super class for all values, <TT>System.Object</TT>
    in our implementation.</P>
  <P>Some values can be directly represented by NGWS objects
    (e.g., <TT>System.Int32</TT>).  Other values are implemented by
    special classes in the <TT>Alice.Values</TT> namespace provided
    by <TT>Alice.dll</TT>.

  <H3>Basic Types</H3>
  <P>Values of basic types are mapped to corresponding NGWS types:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Type<TH>NGWS Representation
      <TR><TD>int<TD><TT>System.Int32</TT>
      <TR><TD>char<TD><TT>System.Char</TT>
      <TR><TD>word<TD><TT>System.Int32</TT>
      <TR><TD>real<TD><TT>System.Double</TT>
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
      <TR><TH>Alice Type<TH>NGWS Representation
      <TR>
	<TD>Unit value <TT>()</TT> resp. <TT>{}</TT>
	<TD><TT>A System.Int32 with value 0</TT>
      <TR><TD>Other record<TD><TT>System.Array</TT>
    </TABLE>
  </CENTER>

  <H3>Procedures</H3>
  <P>All Alice functions are represented by subclasses of
    <TT>Alice.Values.Procedure</TT>.  Conceptually, all Alice functions
    are unary.  Since the typical idiom is to pass multiple arguments as
    a tuple of arguments, this case is treated specially by the Alice
    compiler.  Special classes are provided for this (see below).</P>
  <P><TT>Alice.Values.Procedure</TT> defines an abstract method <TT>Apply</TT>
    in variants that accept 0, ..., 9 arguments of type <TT>System.Object</TT>
    and return a <TT>System.Object</TT>.  Since in a polymorphic language it
    cannot in general be determined statically whether a function takes a
    single record as argument or multiple arguments, only one <TT>Apply</TT>
    method implements the actual body of the function and the others serve
    to perform the necessary runtime calling-convention conversions.</P>
  <P>The actual values of a function's free variables of the function are
    represented in fields of the generated <TT>Alice.Values.Procedure</TT>
    subclass.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Type<TH>NGWS Representation
      <TR>
	<TD>Function taking unit argument
	<TD>Alice.Values.Procedure0
      <TR>
	<TD>Function taking a record with 2, ..., 9 fields
	<TD><TT>Alice.Values.Procedure2</TT>, ...,
	  <TT>Alice.Values.Procedure9</TT>
      <TR><TD>Other function<TD><TT>Alice.Values.Procedure</TT>
    </TABLE>
  </CENTER>


  <H3>Constructors</H3>
  <P>Alice has two kinds of constructors:  constructors of open datatypes
    and constructors of closed datatypes.</P>
  <P>Constructors of open datatypes are generative.  A new GUID is created
    to represent each constructor.  The constructors of the basis library
    are represented as strings.</P>
  <P>Constructors of closed datatypes are statically mapped to integers:
    All constructors of a datatype are sorted lexicographically and are
    allocated consecutive integers starting from&nbsp;0.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Constructor<TH>NGWS Representation
      <TR>
	<TD>User-defined, open datatype
	<TD><TT>System.Guid</TT>
      <TR>
	<TD>Predefined, open datatype
	<TD><TT>System.String</TT>
      <TR>
	<TD>Closed datatype
	<TD><TT>System.Int32</TT>
    </TABLE>
  </CENTER>


  <H3>Constructed Values</H3>
  <P>A constructed value is a value tagged by a constructor, i.e., a
    pair of the constructor and the value.  In the case of a constructor
    of an open datatype, this is a pair of two <TT>System.Object</TT>
    instances; for closed datatypes, this is a pair of an integer and
    a <TT>System.Object</TT>.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Constructor<TH>NGWS Representation
      <TR><TD>Open datatype<TD><TT>Alice.Values.ConVal</TT>
      <TR><TD>Closed datatype<TD><TT>Alice.Values.TagVal</TT>
    </TABLE>
  </CENTER>


  <H3>Futures</H3>
  <P>The implementation distinguishes between several different types of
    future.  Since tests for futures are very frequent in Alice programs,
    they have a common superclass identifying the value as a future.
    This class is <TT>Alice.Values.Transient</TT>; it inherits from
    <TT>Alice.Values.Procedure</TT> to forward calls when the future's
    value is known.</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Future<TH>NGWS Representation
      <TR><TD>Abstract superclass<TD><TT>Alice.Values.Transient</TT>
      <TR><TD>Future<TD><TT>Alice.Values.Future</TT>
      <TR><TD>Assignable portion of future<TD><TT>Alice.Values.Hole</TT>
      <TR><TD>By-need future<TD><TT>Alice.Values.Byneed</TT>
      <TR><TD>Failed future<TD><TT>Alice.Values.FailedTransient</TT>
    </TABLE>
  </CENTER>


  <H3>Other Library Types</H3>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Library Type<TH>NGWS Representation
      <TR><TD><TT>Array.array</TT><TD><TT>Alice.Values.Array</TT>
      <TR><TD><TT>Vector.vector</TT><TD><TT>System.Array</TT>
      <TR><TD><TT>Thread.thread</TT><TD><TT>System.Threading.Thread</TT>
      <TR>
	<TD><TT>TextIO.instream</TT>, <TT>TextIO.outstream</TT>
	<TD><TT>Alice.Values.StreamWrapper</TT>
    </TABLE>
  </CENTER>


  <H3>Modules</H3>
  <P>Alice modules are translated to core data structures such:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Module<TH>NGWS Representation
      <TR><TD>Structure<TD>like records
      <TR><TD>Functor<TD>like functions
    </TABLE>
  </CENTER>
  <P>Structure members are sorted lexicographically.  For this sort,
    the names of module members (structures and functors) are prefixed with
    a dollar sign, while the names of value members (values and constructors)
    remain unchanged.</P>
  <P>Signatures and types are not yet represented at run-time.  Note that
    signature coercions can change structure representations.</P>


  <H3>Components</H3>
  <P>Alice components are DLLs:</P>
  <CENTER>
    <TABLE class=dyptic>
      <TR><TH>Alice Component<TH>NGWS Representation
      <TR><TD>Compiled component<TD>DLL with class <TT>Execute</TT>
    </TABLE>
  </CENTER>
  <P>To be used as a component a DLL must export a class <TT>Execute</TT>
    with a method</P>
  <PRE>
	public static System.Object Main(System.Object Composer)</PRE>
  <P>The argument to <TT>Main</TT> is a handle for the composer (to resolve
    imports) which is an instance of <TT>Alice.Komponist</TT>.
    The result of the method must be the representation of the
    component's body, which is regarded as a single structure.
    Thus, <TT>Main</TT> will always return a <TT>System.Array</TT>.</P>
  <P>Components not written in Alice can be combined with Alice
    components by providing <A href="usage.php3#foreign">suitable
    signatures</A> at compile time, as well as a wrapper adhering to
    the outlied data format.</P>

<?php footing() ?>
