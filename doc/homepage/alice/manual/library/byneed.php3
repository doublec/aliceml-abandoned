<?php include("macros.php3"); ?>
<?php heading("The <TT>ByNeed</TT> functor", "The <TT>ByNeed</TT> functor") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    functor ByNeed (signature S functor F() : S) : S
  </PRE>

  <P>
    The <TT>ByNeed</TT> functor allows arbitrary module expressions to
    be evaluated lazily. For example, if <TT><I>mod</I></TT> is a module
    expression with signature <TT><I>SIG</I></TT>, then evaluation can be
    suspended as follows:
  </P>

  <PRE>
    structure Mod = ByNeed (signature S = <I>SIG</I> functor F() = <I>mod</I>)</PRE>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature S
    functor F () : S
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>signature S</TT>
    </DT>
    <DD>
      <P>The signature of the module to be evaluated lazily.</P>
    </DD>

    <DT>
      <TT>functor F</TT>
    </DT>
    <DD>
      <P>A functorial suspension of the module expression to be evaluated.</P>
    </DD>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="component.php3"><TT>Component</TT></A>
  </DD></DL>

<?php footing() ?>
