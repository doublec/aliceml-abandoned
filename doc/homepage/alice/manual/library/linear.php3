<?php include("macros.php3"); ?>
<?php heading("The Linear structure", "The <TT>Linear</TT> structure"); ?>

<?php section("synopsis", "synopsis"); ?>

  <PRE>
    signature LINEAR
    structure Linear : LINEAR</PRE>

  <P>
    The <TT>Linear</TT> structure provides functionality to post
    linear equality constraints using a convenient infix operator syntax.
  </P>

<?php section("import", "import"); ?>

  <PRE>
    import signature LINEAR from "x-alice:/lib/constraints/LINEAR-sig"
    import structure Linear from "x-alice:/lib/constraints/Linear"</PRE>

<?php section("interface", "interface"); ?>

  <PRE>
    signature LINEAR =
    sig
	datatype domain_element =
	    `` of int
	  | `# of int * int

	type domain = domain_element list

	datatype term =
	    fd of FD.fd
	  | `  of int
	  | `+ of term * term
	  | `- of term * term
	  | `* of term * term

	datatype rel =
	    `<   of term * term
	  | `<=  of term * term
	  | `=   of term * term
	  | `<>  of term * term
	  | `>=  of term * term
	  | `>   of term * term
	  | `<-> of rel * term

	val var : domain option -> term
	val bin : unit -> term
	val vec : int * domain -> term vector

	val distribute : FD.dist_mode * term vector -> unit
	val distinct : term vector -> unit
	val post : rel -> unit

	infix  7  `*
	infix  6  `+ `-
	infix  5  `#
	infix  4  `= `<> `> `>= `< `<= `<->
    end</PRE>

<?php section("description", "description"); ?>

  <DL>
    <DT>
      <TT>datatype domain_element = `` of int | `# of int *int<BR>
          type domain = domain_element list</TT>
    </DT>
    <DD>
      <P>Used to describe domains of finite domain variables. <TT>`` i</TT>
         denotes a single integer value and <TT>>`#(l,h)</TT> denotes all
         integer values between <TT>l</TT> and <TT>h</TT>. For example,
         <TT>[``3,`#(5,10)]</TT> denotes <TT>[3,5,6,7,8,9,10]</TT>.
      </P>
    </DD>

    <DT>
      <TT>datatype term =<BR>
              fd of FD.fd<BR>
            | `  of int<BR>
            | `+ of term * term<BR>
            | `- of term * term<BR>
            | `* of term * term</TT>
    </DT>
    <DD>
      <P>This datatype is used to post arithmetic constraints.
      </P>
      <UL>
        <LI><TT>fd <I>x</I></TT> injects the finite domain
            variable <I>x</I> into a term.
        <LI><TT>` <I>i</I></TT> injects a integer value <I>i</I> into a term.
        <LI><TT>`+(<I>x</I><I>y</I>)</TT> denotes the sum
            of <I>x</I> and <I>y</I>.
        <LI><TT>`-(<I>x</I><I>y</I>)</TT> denotes
            the difference of <I>x</I> and <I>y</I>.
        <LI><TT>`*(<I>x</I><I>y</I>)</TT> denotes the
            product of <I>x</I> and <I>y</I>.
      </UL>
    </DD>

    <DT>
      <TT>datatype rel =<BR>
	    `<   of term * term<BR>
	  | `<=  of term * term<BR>
	  | `=   of term * term<BR>
	  | `<>  of term * term<BR>
	  | `>=  of term * term<BR>
	  | `>   of term * term<BR>
	  | `<-> of rel * term</TT>
    </DT>
    <DD>
      <P>This datatype is used to post linear equations.
      </P>
      <UL>
        <LI><TT>`<(<I>x</I>,<I>y</I>)</TT> denotes that
            <I>x</I> is less than <I>y</I>.
        <LI><TT>`<=(<I>x</I>,<I>y</I>)</TT> denotes that
            <I>x</I> is less than or equal to <I>y</I>.
        <LI><TT>`=(<I>x</I>,<I>y</I>)</TT> denotes that
            <I>x</I> equals <I>y</I>.
        <LI><TT>`<>(<I>x</I>,<I>y</I>)</TT> denotes that
            <I>x</I> is no equal to <I>y</I>.
        <LI><TT>`>=(<I>x</I>,<I>y</I>)</TT> denotes that
            <I>x</I> is greater than or equal to <I>y</I>.
        <LI><TT>`>(<I>x</I>,<I>y</I>)</TT> denotes that
            <I>x</I> is greater than <I>y</I>.
        <LI><TT>`<->(<I>e</I>,<I>c</I>)</TT> reifies
            the equation <I>e</I> into <I>c</I>.
      </UL>
    </DD>

    <DT>
      <TT>var <I>dom</I></TT>
    </DT>
    <DD>
      <P>returns a freshly created finite domain variable term initialized
         with <I>dom</I>.
      </P>
    </DD>

    <DT>
      <TT>bin ()</TT>
    </DT>
    <DD>
      <P>returns a freshly created <TT>0/1</TT> variable term which can be used
         for reification.
      </P>
    </DD>

    <DT>
      <TT>vec (<I>n</I>,<I>dom</I>)</TT>
    </DT>
    <DD>
      <P>returns a vector of size <I>n</I> of
         freshly created finite domain variable terms. Each variable
         is initialized with <I>dom</I>.
      </P>
    </DD>

    <DT>
      <TT>distribute (<I>s</I>,<I>v</I>)</TT>
    </DT>
    <DD>
      <P>distributes the variables in <I>v</I> according to the
         given strategy <I>s</I>. For details, see
         <A HREF="FD.php3">here</A>.
      </P>
    </DD>

    <DT>
      <TT>distinct <I>v</I></TT>
    </DT>
    <DD>
      <P>posts the distrinct constraint on all variables in <I>v</I>.
         For details, see <A HREF="FD.php3">here</A>.
      </P>
    </DD>

    <DT>
      <TT>post <I>r</I></TT>
    </DT>
    <DD>
      <P>post the constraint denoted by <I>r</I>.
      </P>
    </DD>
  </DL>

<?php footing() ?>
