<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 2 - Libraries", "libraries") ?>



<?php section("overview", "overview") ?>

  <P>
    Stockhausen is equipped with parts of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/sml-std-basis.html">Standard
    ML Basis Library</A>.  Additional modules deal with Alice extensions
    over Standard ML.
  </P>

  <P>
    The overall library is structured as follows:
  </P>

  <UL>
    <LI><A href="#toplevel">top-level environment</A>
    <LI><A href="#fundamental">fundamental library</A>
    <LI><A href="#system">system library</A>
    <LI><A href="#utility">utilities library</A>
        (<TT>"x-alice:/lib/utility/..."</TT>)
    <LI><A href="gtk.php3">Gtk library</A>
        (<TT>"x-alice:/lib/gtk/..."</TT>)
    <LI><A href="constraints.php3">constraint library</A>
        (<TT>"x-alice:/lib/constraints/..."</TT>)
    <LI><A href="#tools">tools</A>
        (<TT>"x-alice:/lib/tools/..."</TT>)
  </UL>

  <P>
    The first three are available by default to Alice programs,
    while components from the latter need to be imported
    through URIs using the <TT>x-alice:</TT> scheme.
    For example, a program using the <TT>Url</TT> structure has
    to be prefixed by the following import announcement
    (see <A href="components.php3">components</A>):
  </P>

  <PRE>
	import structure Url from "x-alice:/lib/utility/Url"
  </PRE>


<?php section("toplevel", "top-level") ?>

  <P>
    The Standard ML
    <A href="http://www.dina.kvl.dk/~sestoft/sml/top-level-chapter.html">
    top-level environment</A> has been implemented completely, with the
    caveat that the special purpose procedure <TT>use</TT> is only available
    in the <A href="usage.php3#stot">interactive toplevel</A>.
  </P>

  <P>
    Note however that Stockhausen does not currently support overloading,
    so all overloaded operations in the top-level evironment are only
    available at their default type.
  </P>

  <P>
    Stockhausen adds the functions <TT>byneed</TT> and <TT>concur</TT> to the
    top-level, to provide for easy <A href="laziness.php3">laziness</A> and
    <A href="futures.php3">concurrency with futures</A>.
  </P>


<?php section("fundamental", "fundamental") ?>

  <P>
    The so-called <I>fundamental</I> library of Stockhausen Operette 2
    consists of the resource-free parts of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/sml-std-basis.html">SML
    Basis Library</A>, plus the following additional Alice-specific modules:
  </P>

  <UL>
    <LI> <TT>structure Ref</TT> </LI>
    <LI> <TT>structure VectorPair</TT> </LI>
    <LI> <TT>structure Alt</TT> </LI>
    <LI> <TT>structure <A href="futures.php3#future">Future</A></TT> </LI>
    <LI> <TT>structure <A href="futures.php3#promise">Promise</A></TT> </LI>
    <LI> <TT>structure <A href="futures.php3#cell">Cell</A></TT> </LI>
    <LI> <TT>structure <A href="threads.php3#sig">Thread</A></TT>
    <LI> <TT>structure <A href="packages.php3">Package</A></TT> </LI>
    <LI> <TT>functor <A href="laziness.php3#modules">ByNeed</A></TT> </LI>
  </UL>

  <P>
    The first three modules extend the SML Basis Library by providing
    useful functionality on references, on pairs of vectors (similar to
    the <TT>ListPair</TT> structure) and on the datatype
  </P>

  <PRE>
	datatype ('a,'b) alt = FST of 'a | SND of 'b
  </PRE>

  <P>
    The following fundamental modules correspond to the modules from the
    SML Basis Library with the same name and are fully implemented.
    Some of them - in particular <TT>List</TT>, <TT>Vector</TT> and similar
    structures - even provide considerably extended functionality.
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
  </UL>

  <P>
    Other structures as yet only implement parts of the SML Basis Library's
    functionality:
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
  </UL>


<?php section("system", "system") ?>

  <P>
    The system namespace contains the resourceful modules from the
    SML Basis Library.
    The Stockhausen <A href="pickling.php3">pickling library</A> also
    lives in this namespace.
  </P>
    
  <P>
    Only the following modules from the the SML Basis Library are available
    for now, in incomplete form:
  </P>

  <UL>
    <LI>
      <TT><A name="command-line">structure</A>
      <A href="http://www.dina.kvl.dk/~sestoft/sml/command-line.html">CommandLine</A>
      <BR>from "x-alice:/lib/system/CommandLine"</TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/io.html">IO</A>
      <BR>from "x-alice:/lib/system/IO"</TT>
    </LI>
    <LI>
      <TT><A name="text-io">structure</A>
      <A href="http://www.dina.kvl.dk/~sestoft/sml/text-io.html">TextIO</A>
      <BR>from "x-alice:/lib/system/TextIO"</TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/os.html">OS</A>
      <BR>from "x-alice:/lib/system/OS"</TT>
      <BR>with substructures
      <TT><A href="http://www.dina.kvl.dk/~sestoft/sml/os-file-sys.html"
      >FileSys</A></TT> and
      <TT><A href="http://www.dina.kvl.dk/~sestoft/sml/os-process.html"
      >Process</A></TT>
    </LI>
    <LI>
      <TT>structure
      <A href="http://www.dina.kvl.dk/~sestoft/sml/unix.html">Unix</A>
      <BR>from "x-alice:/lib/system/Unix"</TT>
    </LI>
  </UL>

  <P>
    Structures from the Standard ML Basis Library not listed above are not
    available in Operette 2.
  </P>


<?php section("utility", "utility") ?>

  <UL>
    <LI>
      <TT>structure Url
      <BR>from "x-alice:/lib/utility/Url"</TT>
    </LI>
  </UL>

  and others...


<?php section("tools", "tools") ?>

  <P>
    The only component in the tools namespace currently is the
    <A href="inspector.php3"><I>Alice Inspector</I></A>
    (aka. <I>Data Hauser</I>).
  </P>


<?php footing() ?>
