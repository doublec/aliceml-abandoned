<?php include("macros.php3"); ?>
<?php heading("The Alice library", "") ?>

<?php section("synopsis", "synopsis") ?>

  <P>
    <B>UNFINISHED</B><BR>
    These pages describe the Alice library. It extends the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/">Standard ML
    Basis library</A>.
  </P>

<?php section("toplevel", "top-level") ?>

   <P><TT><A href="toplevel.php3">top-level environment</A></TT></P>

<?php section("signatures", "signatures") ?>

  <P><TT>lib/fundamental</TT>:</P>

  <TT>
  <TABLE>
    <TR> <TD><A href="alt.php3">ALT</A></TD> </TR>
    <TR> <TD><A href="array.php3">ARRAY</A></TD> </TR>
    <TR> <TD><A href="bool.php3">BOOL</A></TD> </TR>
    <TR> <TD><A href="char.php3">CHAR</A></TD> </TR>
    <TR> <TD><A href="future.php3">FUTURE</A></TD> </TR>
    <TR> <TD><A href="general.php3">GENERAL</A></TD> </TR>
    <TR> <TD><A href="ieee-real.php3">IEEE_REAL</A></TD> </TR>
    <TR> <TD><A href="integer.php3">INTEGER</A></TD> </TR>
    <TR> <TD><A href="list.php3">LIST</A></TD> </TR>
    <TR> <TD><A href="list-pair.php3">LIST_PAIR</A></TD> </TR>
    <TR> <TD><A href="lock.php3">LOCK</A></TD> </TR>
    <TR> <TD><A href="math.php3">MATH</A></TD> </TR>
    <TR> <TD><A href="mono-array.php3">MONO_ARRAY</A></TD> </TR>
    <TR> <TD><A href="mono-vector.php3">MONO_VECTOR</A></TD> </TR>
    <TR> <TD><A href="option.php3">OPTION</A></TD> </TR>
    <TR> <TD><A href="pair.php3">PAIR</A></TD> </TR>
    <TR> <TD><A href="real.php3">REAL</A></TD> </TR>
    <TR> <TD><A href="ref.php3">REF</A></TD> </TR>
    <TR> <TD><A href="string.php3">STRING</A></TD> </TR>
    <TR> <TD><A href="substring.php3">SUBSTRING</A></TD> </TR>
    <TR> <TD><A href="thread.php3">THREAD</A></TD> </TR>
    <TR> <TD><A href="time.php3">TIME</A></TD> </TR>
    <TR> <TD><A href="unique-string.php3">UNIQUE_STRING</A></TD> </TR>
    <TR> <TD><A href="vector.php3">VECTOR</A></TD> </TR>
    <TR> <TD><A href="vector-pair.php3">VECTOR_PAIR</A></TD> </TR>
    <TR> <TD><A href="word.php3">WORD</A></TD> </TR>
  </TABLE>
  </TT>

  <P><TT>lib/system</TT>:</P>

  <TT>
  <TABLE>
    <TR> <TD><A href="component.php3">COMPONENT</A></TD> </TR>
    <TR> <TD><A href="component-manager.php3">COMPONENT_MANAGER</A></TD> </TR>
    <TR> <TD><A href="config.php3">CONFIG</A></TD> </TR>
    <TR> <TD><A href="http.php3">HTTP</A></TD> </TR>
    <TR> <TD><A href="http-client.php3">HTTP_CLIENT</A></TD> </TR>
    <TR> <TD><A href="http-server.php3">HTTP_SERVER</A></TD> </TR>
    <TR> <TD><A href="resolver.php3">RESOLVER</A></TD> </TR>
    <TR> <TD><A href="resolver-handler.php3">RESOLVER_HANDLER</A></TD> </TR>
    <TR> <TD><A href="socket.php3">SOCKET</A></TD> </TR>
    <TR> <TD><A href="url.php3">URL</A></TD> </TR>
  </TABLE>
  </TT>

<?php section("structures", "structures") ?>

  <P><TT>lib/fundamental</TT>:</P>

  <TT>
  <TABLE>
    <TR> <TD><A href="alt.php3">Alt</A></TD> </TR>
    <TR> <TD><A href="array.php3">Array</A></TD> </TR>
    <TR> <TD><A href="bool.php3">Bool</A></TD> </TR>
    <TR> <TD><A href="char.php3">Char</A></TD> </TR>
    <TR> <TD><A href="mono-array.php3">CharArray</A></TD> </TR>
    <TR> <TD><A href="mono-vector.php3">CharVector</A></TD> </TR>
    <TR> <TD><A href="future.php3">Future</A></TD> </TR>
    <TR> <TD><A href="general.php3">General</A></TD> </TR>
    <TR> <TD><A href="ieee-real.php3">IEEEReal</A></TD> </TR>
    <TR> <TD><A href="integer.php3">Int</A></TD> </TR>
    <TR> <TD><A href="integer.php3">LargeInt</A></TD> </TR>
    <TR> <TD><A href="real.php3">LargeReal</A></TD> </TR>
    <TR> <TD><A href="word.php3">LargeWord</A></TD> </TR>
    <TR> <TD><A href="list.php3">List</A></TD> </TR>
    <TR> <TD><A href="list-pair.php3">ListPair</A></TD> </TR>
    <TR> <TD><A href="lock.php3">Lock</A></TD> </TR>
    <TR> <TD><A href="math.php3">Math</A></TD> </TR>
    <TR> <TD><A href="option.php3">Option</A></TD> </TR>
    <TR> <TD><A href="pair.php3">Pair</A></TD> </TR>
    <TR> <TD><A href="integer.php3">Position</A></TD> </TR>
    <TR> <TD><A href="real.php3">Real</A></TD> </TR>
    <TR> <TD><A href="ref.php3">Ref</A></TD> </TR>
    <TR> <TD><A href="string.php3">String</A></TD> </TR>
    <TR> <TD><A href="substring.php3">Substring</A></TD> </TR>
    <TR> <TD><A href="thread.php3">Thread</A></TD> </TR>
    <TR> <TD><A href="time.php3">Time</A></TD> </TR>
    <TR> <TD><A href="unique-string.php3">UniqueString</A></TD> </TR>
    <TR> <TD><A href="vector.php3">Vector</A></TD> </TR>
    <TR> <TD><A href="vector-pair.php3">VectorPair</A></TD> </TR>
    <TR> <TD><A href="char.php3">WideChar</A></TD> </TR>
    <TR> <TD><A href="string.php3">WideString</A></TD> </TR>
    <TR> <TD><A href="substring.php3">WideSubstring</A></TD> </TR>
    <TR> <TD><A href="unique-string.php3">WideUniqueString</A></TD> </TR>
    <TR> <TD><A href="word.php3">Word</A></TD> </TR>
    <TR> <TD><A href="word.php3">Word8</A></TD> </TR>
    <TR> <TD><A href="word.php3">Word31</A></TD> </TR>
    <TR> <TD><A href="mono-array.php3">Word8Array</A></TD> </TR>
    <TR> <TD><A href="mono-vector.php3">Word8Vector</A></TD> </TR>
  </TABLE>
  </TT>

  <P><TT>lib/system</TT>:</P>

  <TT>
  <TABLE>
    <TR> <TD><A href="component.php3">Component</A></TD> </TR>
    <TR> <TD><A href="config.php3">Config</A></TD> </TR>
    <TR> <TD><A href="http.php3">Http</A></TD> </TR>
    <TR> <TD><A href="http-client.php3">HttpClient</A></TD> </TR>
    <TR> <TD><A href="http-server.php3">HttpServer</A></TD> </TR>
    <TR> <TD><A href="resolver.php3">Resolver</A></TD> </TR>
    <TR> <TD><A href="socket.php3">Socket</A></TD> </TR>
    <TR> <TD><A href="url.php3">Url</A></TD> </TR>
  </TABLE>
  </TT>

<?php section("functors", "functors") ?>

  <P><TT>lib/fundamental</TT>:</P>

  <TT>
  <TABLE>
    <TR> <TD><A href="byneed.php3">ByNeed</A></TD> </TR>
  </TABLE>
  </TT>

<?php footing() ?>
