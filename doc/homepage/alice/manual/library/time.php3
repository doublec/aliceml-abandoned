<?php include("macros.php3"); ?>
<?php heading("The Time structure", "The <TT>Time</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature TIME
    structure Time : TIME
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/time.html">Standard ML
    Basis' <TT>Time</TT></A> structure.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature TIME =
    sig
	eqtype time
	type t = time

	exception Time

	val zeroTime :         time

	val fromReal :         LargeReal.real -> time
	val toReal :           time -> LargeReal.real
	val toSeconds :        time -> LargeInt.int
	val toMilliseconds :   time -> LargeInt.int
	val toMicroseconds :   time -> LargeInt.int
	val fromSeconds :      LargeInt.int -> time
	val fromMilliseconds : LargeInt.int -> time
	val fromMicroseconds : LargeInt.int -> time

	val op + :             time * time -> time
	val op - :             time * time -> time
	val op < :             time * time -> bool
	val op > :             time * time -> bool
	val op <= :            time * time -> bool
	val op >= :            time * time -> bool
	val equal :            time * time -> bool
	val compare :          time * time -> order
	val hash :             time  -> int
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/time.html">Standard ML
    Basis' <TT>Time</TT></A> structure.
  </P>

  <P>
    <I>Limitations:</I> The following standard functions are
    currently missing:
  </P>

  <UL>
    <LI><TT>now</TT></LI>
    <LI><TT>fromString</TT>, <TT>toString</TT>, <TT>fmt</TT>, <TT>scan</TT></LI>
  </UL>

  <DL>
    <DT>
      <TT>type t = time</TT>
    </DT>
    <DD>
      <P>A local synonym for type <TT>time</TT>.</P>
    </DD>

    <DT>
      <TT>equal (<I>t1</I>, <I>t2</I>)</TT>
    </DT>
    <DD>
      <P>An explicit equality function on time values. Equivalent to <TT>op=</TT>.</P>
    </DD>

    <DT>
      <TT>hash <I>t</I></TT>
    </DT>
    <DD>
      <P>A hash function for time values.</P>
    </DD>
  </DL>

<?php footing() ?>
