<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 2 - Components", "components") ?>



<?php section("overview", "overview") ?>

  <P>
    SML does not deal with separate compilation. Alice repairs
    this by adding a component system to the language. Alice
    components were inspired by Mozart "functors". They support lazy linking
    and loading from remote URLs, but add static and dynamic type checking.
    Components are architecture independent and can be executed on any system.
  </P>

  <P>
    A component is an SML program headed by a set of <I>import
    announcements</I> of the form:
  </P>

  <PRE>
	import <I>imp</I> from "url"
  </PRE>

  <P>
    The URL describes where to find the component, while the import <I>imp</I>
    denotes what kind of item is imported from that particular component.
    For example,
  </P>

  <PRE>
	import structure Foo from "http://ps.uni-sb.de/stockhausen/Foo"
  </PRE>

  <P>
    An import can contain any language entity, like values, types, structures,
    signatures, etc. Imports can be given in plain form like above (in which
    case the compiler looks up the actual type of the entity in the component
    itself) or in description form similar to specifications (in which case
    the compiler verifies that the description matches the actual component):
  </P>

  <PRE>
	import signature FOO       from "http://ps.uni-sb.de/stockhausen/FOO-sig"
	import structure Foo : FOO from "http://ps.uni-sb.de/stockhausen/Foo"
  </PRE>

  <P>
    Each component exports exactly its body environment, i.e. all entities
    declared on its toplevel. For example, the component
  </P>

  <PRE>
	import structure Y from "other"

	signature S = sig end
	structure X :> S = struct end
  </PRE>

  <P>
	enables other components to import a signature <TT>S</TT> and a
	structure <TT>X</TT> from it. Structure <TT>Y</TT> is not exported
	(but can be by a simple rebinding, of course).
  </P>



<?php section("execution", "execution") ?>

  <P>
    A Stockhausen program is executed by starting a component. In general, a
    component relies on other components. These other components are loaded
    and evaluated by need by the <I>Komponist</I>, who is part of the runtime
    system.
    A component that is never actually accessed at runtime will not get loaded.
    URL resolving is handled similar to Mozart.
  </P>

  <P>
    When a component is loaded, its signature is matched against the signature
    expected by the dependent component - the signature that was found when
    the dependent component was compiled. A runtime exception is raised
    when the signature does not match. If the check was successful, the
    component is executed in a separate thread.
  </P>

  <P>
    Every component is loaded and executed at most once in a single process.
    If several other components load a component from the same URL, they will
    share a reference to the same instantiation of that component.
  </P>



<?php section("syntax", "syntax") ?>

  <TABLE>
    <TR>
      <TD> <I>component</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>ann</I> &lt;<I>program</I>&gt; </TD>
      <TD> component </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>ann</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>import</TT> <I>imp</I> <TT>from</TT> <I>string</I> </TD>
      <TD> import announcement </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD>  </TD>
      <TD> empty </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>ann</I> &lt;<TT>;</TT>&gt; <I>ann</I> </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>imp</I> </TD>
      <TD align="center">::=</TD>
      <TD> <TT>val</TT> <I>valitem</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>type</TT> <I>typitem</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>datatype</TT> <I>datitem</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>constructor</TT> <I>dconitem</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>exception</TT> <I>exitem</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>structure</TT> <I>stritem</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>functor</TT> <I>funitem</I> </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>signature</TT> <I>sigitem</I> </TD>
      <TD> </TD>
    </TR>
    <TR valign=baseline>
      <TD></TD> <TD></TD>
      <TD> <TT>infix</TT> &lt;<I>d</I>&gt; <I>vid</I><SUB>1</SUB>
                                       ... <I>vid</I><SUB>n</SUB> </TD>
      <TD> (n>=1)</TD>
    </TR>
    <TR valign=baseline>
      <TD></TD> <TD></TD>
      <TD> <TT>infixr</TT> &lt;<I>d</I>&gt; <I>vid</I><SUB>1</SUB>
                                        ... <I>vid</I><SUB>n</SUB> </TD>
      <TD> (n>=1)</TD>
    </TR>
    <TR valign=baseline>
      <TD></TD> <TD></TD>
      <TD> <TT>nonfix</TT> <I>vid</I><SUB>1</SUB>
                       ... <I>vid</I><SUB>n</SUB> </TD>
      <TD> (n>=1)</TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD>  </TD>
      <TD> empty </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>imp</I> &lt;<TT>;</TT>&gt; <I>imp</I> </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>valitem</I> </TD>
      <TD align="center">::=</TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I>
           &lt;<TT>and</TT> <I>valitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I> <TT>:</TT> <I>ty</I>
           &lt;<TT>and</TT> <I>valitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>typitem</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>tycon</I> &lt;<TT>and</TT> <I>typitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>tyvarseq</I> <I>tycon</I>
           &lt;<TT>and</TT> <I>typitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>datitem</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>tycon</I> &lt;<TT>and</TT> <I>datitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>tyvarseq</I> <I>tycon</I> <TT>=</TT> <I>conitem</I>
           &lt;<TT>and</TT> <I>datitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>conitem</I> </TD>
      <TD align="center">::=</TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I> &lt;<TT>of</TT> <I>ty</I>&gt;
           &lt;<TT>|</TT> <I>conitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>dconitem</I> </TD>
      <TD align="center">::=</TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I> 
           &lt;<TT>and</TT> <I>dconitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I> &lt;<TT>of</TT> <I>ty</I>&gt;
	   <TT>:</TT> <I>tyvarseq</I> <I>longtycon</I>
           &lt;<TT>and</TT> <I>dconitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>exitem</I> </TD>
      <TD align="center">::=</TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I> 
           &lt;<TT>and</TT> <I>exitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid</I> <TT>of</TT> <I>ty</I>
           &lt;<TT>and</TT> <I>exitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>stritem</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>strid</I> &lt;<TT>and</TT> <I>stritem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <I>strid</I> <TT>:</TT> <I>sigexp</I>
           &lt;<TT>and</TT> <I>stritem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>funitem</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>strid</I> &lt;<TT>and</TT> <I>funitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR valign=baseline>
      <TD></TD> <TD></TD>
      <TD> <I>strid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB>n</SUB>
	   <TT>:</TT> <I>sigexp</I>
           &lt;<TT>and</TT> <I>funitem</I>&gt; </TD>
      <TD> (n>=1) </TD>
    </TR>
    <TR></TR>
    <TR>
      <TD> <I>sigitem</I> </TD>
      <TD align="center">::=</TD>
      <TD> <I>sigid</I> &lt;<TT>and</TT> <I>sigitem</I>&gt; </TD>
      <TD> </TD>
    </TR>
    <TR valign=baseline>
      <TD></TD> <TD></TD>
      <TD> <I>sigid</I> <I>strpat</I><SUB>1</SUB> ... <I>strpat</I><SUB>n</SUB>
           &lt;<TT>and</TT> <I>sigitem</I>&gt; </TD>
      <TD> (n>=1) </TD>
    </TR>
  </TABLE>

  <P>
    Note that exception and functor imports are actually derived forms.
  </P>


<?php footing() ?>
