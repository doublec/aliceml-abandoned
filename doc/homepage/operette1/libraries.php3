<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 1 - Libraries",
		"libraries") ?>



<?php section("overview", "overview") ?>

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



<?php section("toplevel", "top-level") ?>

  <P>
    The Standard ML
    <A href="http://www.dina.kvl.dk/~sestoft/sml/top-level-chapter.html">
    top-level environment</A> has been implemented almost completely, except
    <TT>print</TT> (you have to import it from <TT>TextIO</TT>) and the
    special purpose procedure <TT>use</TT>.
  </P>

  <P>
    Note however that Stockhausen does not currently support overloading,
    so all overloaded operations in the top-level evironment are only
    available at their default type.
  </P>



<?php section("basis", "basis library") ?>

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
      <A href="http://www.dina.kvl.dk/~sestoft/sml/vector.html">Vector</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/array.html">Array</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/math.html">Math</A>
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
      <A href="http://www.dina.kvl.dk/~sestoft/sml/ieee-float.html">IEEEReal</A>
      </TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/time.html">Time</A>
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
    Structures from the Standard ML Basis Library not listed above are not
    available in Operette 1.
  </P>

  <P>
    Some of the above structures must be imported explicitly. A program using
    the <TT>IO</TT> structure, for example, has to be
    prefixed by the following import announcement
    (see <A href="components.php3">components</A>):
  </P>

  <PRE>
	import structure IO from "x-alice:/lib/IO.ozf"
  </PRE>


<?php footing() ?>
