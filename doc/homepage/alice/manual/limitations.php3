<?php include("macros.php3"); ?>

<?php heading("Packages", "packages") ?>



<?php section("overview", "overview") ?>

<P>For particular programming tasks - especially when transferring date between
different processes - it is desirable to relax the strict static rules of ML
and shift some of the type checking to runtime. Alice hence provides runtime
typing through the concept of <EM>packages</EM>.

<P>A package is a value of abstract type <TT>package</TT> that encapsulates
some value, along with its (runtime) type. Accessing the value inside a package
is only possible via an explicit unpack operation, which requires giving the
expected type. A dynamic type check takes place to verify that the actual and
the expected type match.</P>

<P>Instead of just values of the core language, packages may actually contain
arbitrary <A href="modules.php3">higher-order modules</A>, paired with their
signature. This way, modules can be passed and stored as first-class values.</P>


<?php section("package", "packages") ?>

<?php subsection("package-pack", "Creating packages") ?>

<P>A package is created using the <TT>pack</TT> expression:

<PRE class=code>
pack <I>longstrid</I> :> <I>longsigid</I></PRE>

<P>This expression creates a package containing the module denoted by
<TT><I>longstrid</I></TT>. The module must (statically) match the given
signature <TT><I>longsigid</I></TT>. This signature will be the <EM>package
signature</EM>.</P>


<?php subsection("package-unpack", "Unpacking packages") ?>

<P>A module can be projected out of a package using the inverse <TT>unpack</TT>
form:</P>

<PRE class=code>
unpack <I>exp</I> : <I>sigexp</I></PRE>

<P>Dynamically, if the package signature of the package returned by expression
<TT><I>exp</I></TT> matches the signature <TT><I>sigexp</I></TT>, this
structure expression evaluates to the module encapsulated in the package.
Otherwise, it raises the exception <TT>Package.Mismatch</TT>. Since Alice ML
supports <A href="modules.php3#local">local modules</A>, the exception may be
handled in the usual way.</P>

<P>Statically, the expression has the signature type denoted by
<TT><I>sigexp</I></TT>, as it can only be successfully evaluated if it is able
to deliver a suitable module. Any use of the unpacked module will be statically
type-safe.</P>


<?php subsection("package-example", "Example") ?>

<P>We can inject the library module <TT>Word8</TT> into a package:</P>

<PRE class=code>
val p = pack Word8 :> WORD</PRE>

<P>This package may then be passed to some client who may unpack it as
follows:</P>

<PRE class=code>
structure Word' = unpack p : WORD</PRE>

<P>The package signature matches the expected signature, so that unpacking
succeeds. The client may thus access the <TT>Word'</TT> structure as usual:</P>

<PRE class=code>
print (Word'.toString (Word'.fromInt 255))</PRE>

<P>The package can also be unpacked using a more general signature that contains
fewer items:</P>

<PRE class=code>
structure WordFromInt = unpack p : sig type word  val fromInt : int -> word end</PRE>

<P>The full module subtyping rules apply to the unpack type check, so that
unpacking is quite robust with respect to extensions to interfaces of modules
packed elsewhere. In particular, arbitrary items may be added.</P>


<?php subsection("package-sharing", "Sharing") ?>

<P>The package signature of <TT>p</TT> is <TT>WORD</TT>. That signature does not
specify anything about the identity of the contained <TT>word</TT> type.
Consequently, it will be abstract and incompatible with the original type after
unpacking:</P>

<PRE class=code>
Word'.toString (Word8.fromInt 255)   (* static type error! *)</PRE>

<P>In order to make the package signature compatible with <TT>Word8</TT>, it has
to be specified more specifically:</P>

<PRE class=code>
val p = pack Word8 :> (WORD where type word = Word8.t)</PRE>

<P>Now the package can be unpacked similarly as</P>

<PRE class=code>
structure Word8' = unpack p : (WORD where type word = Word8.t)</PRE>

<P>enabling</P>

<PRE class=code>
Word8'.toString (Word8.fromInt 255)</PRE>

<P>Due to subtyping, it is still valid to unpack it abstractly:</P>

<PRE class=code>
structure Word' = unpack p : WORD</PRE>

<P>but of course, <TT>Word'.word</TT> again is statically different from
<TT>Word8.word</TT>.

<P>Type constraints can also be used to specify sharing between different
packages:</P>

<PRE class=code>
fun g(p1, p2) =
    let
	signature S  = sig type t  val x : t  val f : t -> int end
	structure X1 = unpack p1 : S
	structure X2 = unpack p2 : (S where type t = X1.t)
    in
	X2.f X1.x
    end</PRE>

<P>In this example, the types <TT>t</TT> in both packages are unknown. However,
they might be known to be equal. The second <TT>unpack</TT> expression enforces
this requirement dynamically, by specifying the necessary type equivalence.</P>


<?php section("firstclass", "first-class modules") ?>

<P>The main application for first-class modules in the form of packages is
in combination with <A href="pickling.php3">pickling</A> where they allow making
implementations of abstract types or whole programs persistent, and with <A
href="distribution.php3">distributed programming</A>, where they enable
transfer of program components to remote locations.</A>

<P>Packages can also be used for encoding more traditional idioms requiring
first-class modules. For example, the implementation of a map module may be
chosen dependent on some condition, assuming the alternative implementations
satisfy a common signature:</P>

<PRE class=code>
structure Map = unpack (if maxElems < 100
                        then pack BinTreeMap :> MAP)
			else pack HashTableMap :> MAP) : MAP</PRE>

<!--
<P>Another application is to use structures for object-oriented style
programming. Consider there is a signature</P>

<PRE class=code>
signature SHAPE =
sig
    type t
    type vec = int * int
    val self : t
    val pos :  t -> vec
    val move : t * vec -> unit
end</PRE>

<P>and several functors</P>

<PRE class=code>
functor Rect    (val h : int val w : int) : sig include SHAPE (* ... *) end
functor Circle  (val r : int)             : sig include SHAPE (* ... *) end
functor Polygon (val ps : vec list)       : sig include SHAPE (* ... *) end</PRE>

<P>These functors can be thought of as classes, while the structures they
return are objects of the corresponding classes. By packing object structures
they can be stored in a list, for example:</P>

<PRE class=code>
val shapes = [pack Rect :> SHAPE, pack Circle :> SHAPE, pack Polygon :> SHAPE]</PRE>

<P>It is possible to iterate over <TT>l</TT> without knowing the concrete object
types:</P>

<PRE class=code>
List.app (fn p => let structure S = unpack p : SHAPE
                  in S.move (S.self, (~4,~2))) shapes</PRE>
-->


<?php section("dynamic", "runtime typing") ?>

<P>Packages rely on dynamic typing, thus types (and signatures) have a dynamic
semantics. It is an important property of the semantics that it does not
undermine any properties of the static type system. In particular, all
abstraction guarantees carry over from static to dynamic typing: no type
abstraction can be broken by use of the dynamic typing implied by packages. For
example, the following declarations will not evaluate successfully:</P>

<PRE class=code>
signature S      = sig    type t       val x : t end
structure X :> S = struct type t = int val x = 9 end

val p = pack X :> S

structure X' = unpack p : S where type t = int</PRE>

<P>The unpacking as shown here will always fail, since the type <TT>X.t</TT> is
dynamically abstract. The only possible type that <TT>t</TT> can be specified
equal to is the type from the package itself (see <A
href="#package-sharing">sharing</A>):</P>

<PRE class=code>
structure X1 = unpack p : S
structure X2 = unpack p : S where type t = X1.t</PRE>

<P>The dynamic semantics of types is as follows:</P>

<UL>

<LI>All <EM>explicitly named</EM> types and signatures have a dynamic
representation. In particular, structures carry the contained type
fields (and <A href="modules.php3#sigmembers">signature fields</A>). This implies
that types are passed to and returned from functors.</LI>

<LI>Evaluation of a <EM>generative</EM> type declaration (e.g. <TT>exttype</TT>)
creates a fresh type name.</LI>

<LI><EM>Structure sealing</EM> is generative as well: a structure expression of
the form <TT><I>strexp</I> :> <I>sigexp</I></TT> creates a new structure,
in which all type fields from <TT><I>strexp</I></TT> are replaced
according to <TT><I>sigexp</I></TT>. For every abstract type (and <A
href="modules.php3#sigmembers-abstract">abstract signature</A>) contained in
<TT><I>sigexp</I></TT>, a fresh name is generated and substituted. Nested modules
are sealed recursively.</LI>

<LI>For functors, sealing creates a new functor that seals its result module
upon application.</LI>

<LI>Packages contain the dynamic representation of their signature.</LI>

<LI>Packing always seals the packaged module (as suggested by the <TT>:></TT>
syntax).</LI>

<LI>Unpacking performs a dynamic signature match between the package signature
and the given target signature. In the case of success, it returns the packaged
module unchanged, otherwise raises an exception.</LI>

</UL>

<P>Note that polymorphic functions are still interpreted in a type erasure
semantics. To pass type information, a function has to be lifted to the functor
level. Due to <A href="modules.php3#local">local modules</A> this is possible
most of the times.</P>

<P>Packing performs sealing to ensure consistency between the package signature
and dynamic types contained in the packaged module. If no sealing took place,
certain anomalies would be possible, like successful evaluation of the following
program:</P>

<PRE class=code>
signature S = sig    type t       val x : t end
structure X = struct type t = int val x = 9 end

val p1 = pack X :> S
structure X1 = unpack p1 : S

val p2 = pack X1 :> S where type t = X1.t
structure X2 = unpack p2 : S where type t = int</PRE>

<P>Here, <TT>X</TT> has a transparent type <TT>t</TT> <TT>=</TT> <TT>int</TT>,
but the package signature makes it abstract. Consequently, unpacking it under
the constraint <TT>t</TT> <TT>=</TT> <TT>int</TT> would be unsuccessful.
However, if packing would not perform sealing, <TT>X1.t</TT> indeed was
<TT>int</TT> dynamically, so that an additional packing/unpacking step sufficed
to reveal the implementation type. A package could leak information that was
not appearent from its signature.</P>

<P>On the other hand, unpacking is transparent. The following program evaluates
successfully:</P>

<PRE class=code>
signature S = sig    type t       val x : t end
structure X = struct type t = int val x = 9 end

val p1 = pack X :> S where type t = int
structure X1 = unpack p1 : S

val p2 = pack X1 :> S where type t = X1.t
structure X2 = unpack p2 : S where type t = int</PRE>

<P>In this case, the second <TT>pack</TT> expression makes the signature of
<TT>p2</TT> transparent relative to <TT>p1</TT>, without knowing the actual
type <TT>t</TT> contained in <TT>p1</TT>. Note that it is not possible to
specify <TT>t</TT> <TT>=</TT> <TT>int</TT> at this point, because statically,
<TT>X1.t</TT> is abstract since it has been unpacked abstractly.</P>

<P>Transparent semantics of unpacking are necessary to allow expressing <A
href="#packages-sharing">sharing</A>.</P>


<!--
<?php subsection("dynamic-analysis", "Dynamic type analysis") ?>

<P>Packages can be abused to investigate types dynamically. This is particularly
interesting for functor arguments:</P>

<PRE class=code>
functor GetMonoVector(type t) =
    let
	val t = pack (type t = t) :> (type t = t)
	val p = let structure _ = unpack t : (type t = char) in
		    pack CharVector :> (MONO_VECTOR where type elem = char) end
		handle Package.Mismatch _ =>
		let structure _ = unpack t : (type t = Word8.t) in
		    pack Word8Vector :> (MONO_VECTOR where type elem = Word8.t) end
		handle Package.Mismatch _ =>
		    pack (type elem = t
			  open Vector
			  type vector = elem vector
			  type t = vector
			  fun contains eq xs y = exists (fn x => x = y) xs
			  fun notContains eq xs = not o contains eq xs) :>
			 (MONO_VECTOR where type elem = t)
    in
	unpack p : MONO_VECTOR where type elem = t
    end</PRE>
-->


  <P>
    Packages are provided through the structure
    <TT>Package</TT> which contains several functors:
  </P>

  <PRE>
	structure Package :
	sig
	    type package
	    type t = package

	    type mismatch
	    exception Mismatch of mismatch

	    functor Pack(signature S structure X : S) : (val package : package)
	    functor Unpack(val package : package signature S) : S

	    (* ... *)
	end
  </PRE>

  <P>
    By applying the <TT>Pack</TT> functor one can inject an arbitrary
    module into a package. For example:
  </P>

  <PRE>
	structure P = Package.Pack(structure X = Int64
	                           signature S = INTEGER)
  </PRE>

  <P>
    This functor application creates a package containing the complete
    structure <TT>Int64</TT>, specifying its signature as
    <TT>INTEGER</TT>. A less specific signature may be given, but the
    functor type enforces that the module matches that signature. This
    guarantees that the package is always typed consistently.
  <P>

  <P>
    Note that in this example <TT>INTEGER</TT> in fact is not the
    most specific signature possible. More specific would be
  </P>

  <PRE>
	signature S = INTEGER where type int = Int64.int
  </PRE>

  <P>
    Not specifying this equivalence for the package signature
    actually makes the type int in the package abstract. The original
    type cannot be regained in any way. It is thus best to think of
    this application as
  </P>

  <PRE>
	structure P = Package.Pack(structure X = Int64 :> INTEGER
	                           signature S = INTEGER)
  </PRE>

  <P>
    The resulting structure contains a single value <TT>package</TT>,
    which is a first-class core value. You can pass it to a function, for
    example:
  <P>

  <PRE>
	val x = foo(P.package)
  </PRE>

  <P>
    The function <TT>foo</TT> might be defined as follows:
  </P>

  <PRE>
	(* foo : package -> int *)
	fun foo p =
	    let
		structure MyInt = Package.Unpack(val package = p
		                                 signature S = INTEGER)
		val n = MyInt.*(MyInt.fromInt 1000, MyInt.fromInt 876)
	    in
		MyInt.toInt n
	    end
	    handle Package.Mismatch _ => 0
  </PRE>

  <P>
    Here, the package is unpacked via the functor <TT>Unpack</TT>. By
    specifying the argument signature <TT>S</TT> appropriately, the signature
    of <TT>MyInt</TT> is statically known to be <TT>INTEGER</TT>.
  </P>

  <P>
    If the package does not contain a module that matches the signature
    passed to <TT>Unpack</TT>, the functor application will raise an
    exception <TT>Mismatch</TT>. If desired, this exception can be
    handled in the usual way, as seen in the example. The argument
    to <TT>Mismatch</TT> is an abstract description of the reason for
    the match failure. Currently, this cannot be investigated.
  </P>

  <P>
    Note that packing is not restricted to structures: since Alice
    provides a true <A href="modules.php3">higher-order module system</A>,
    functors can be packed in the same way.
  </P>


<?php section("valpackage", "value packages") ?>

  <P>
    Often it is not necessary to package whole modules. For convenience,
    the <TT>Package</TT> module thus provides functors that allow packaging
    of simple core values. Essentially, this provides a type dynamic, as
    available in a few other languages.
  </P>

  <P>
    The respective functors are the following:
  </P>

  <PRE>
	structure Package :
	sig
	    (* ... *)

	    type val_package
 
	    exception MismatchVal
 
	    functor PackVal(type t val x : t) : (val package : val_package)
	    functor UnpackVal(val package : val_package type t) : (val x : t)
	end
  </PRE>

  <P>
    These are essentially just short hands for special cases of the more
    heavy-weight general functors. Their use is similar. Instead of the
    general <TT>Mismatch</TT> exception, the simpler <TT>MismatchVal</TT>
    is raised in case of type errors during unpacking.
  </P>


<?php section("limitations", "limitations") ?>

  <P>
    The package mechanism in Alice (as well as the
    <A href="pickling.php3">pickling</A> machinery) relies on runtime
    representations of types and signatures. Every type or signature
    declaration in Alice not only has a static semantics, but also
    a dynamic semantics (which closely mirrors the static one). Unlike
    SML there is no type erasure performed before evaluation.
  </P>

  <P>
    In particular, abstraction via <TT>:&gt;</TT> has an operational
    semantics (and cost) in Alice, as it performs abstraction on runtime
    types.
  </P>

  <P>
    As an implementation limitation of the current Alice system, the operational
    semantics of abstraction are currently not correctly implemented
    for cases where the signature is a parameter and thus not known
    statically. In that case, no runtime abstraction on type fields
    is performed. A simple example is:
  </P>

  <PRE>
	functor Abs(signature S structure X : S) = X :> S

	structure M  = struct type t = int val x = 13 end
	structure M' = Abs(signature = sig type t val x : t end
	                   structure = M)
  </PRE>

  <P>
    In this example, the runtime representation of <TT>M'.t</TT>
    will still be <TT>int</TT> and not an abstract type.
    However, since the static semantics work correctly, this can
    only be observed by going through a package:
  </P>

  <PRE>
	structure P = Package.PackVal(type t = M'.t val x = M'.x)
	structure X = Package.UnpackVal(val package = P.package type t = int)

	val y = X.x + 1    (* Aha! *)
  </PRE>

  <P>
    Unpacking should fail in this example, but currently does not.
  </P>

  <P>
    Since abstraction during packing uses the same mechanism,
    packing does not properly treat abstract types either.
    Unfortunately, this property does not only lead to a too permissive
    treatment of abstract types, but can also cause failure for
    examples that should work. Consider:
  </P>

  <PRE>
	structure A0 = struct type t = int val x = 9 end
	signature S  = sig type t val x : t end
	structure P  = Package.Pack(structure X = A0 signature S = S)

	structure A1 = Package.Unpack(val package = P.package
				      signature S = S)
	structure A2 = Package.Unpack(val package = P.package
				      signature S = S where type t = A1.t)
  </PRE>

  <P>
    The second unpack uses the type found when unpacking the first time.
    This way, A1.t and A2.t should be the same, so that A1.x and A2.x are
    compatible. One would expect that the second unpacking always succeeds
    since we are unpacking the same package and already know its type
    <TT>t</TT>. However, since no abstraction is performed on <TT>t</TT>
    during packing, <TT>A1.t</TT> actually is <TT>int</TT>. So we are
    trying to unpack with <TT>t = int</TT>. But the signature stored in the
    package does not specify this equivalence, so signature matching will
    fail during unpacking.
  </P>

  <P>
    The problem is that the package signature and the runtime types
    contained in the package module are inconsistent, if the signature
    passed to <TT>Pack</TT> does not declare the most specific types. So
    a simple workaround is to avoid this situation. For our example, this
    can be done as follows:
  </P>

  <PRE>
	structure P = Package.Pack(structure X = A0
				   signature S = S where type t = X.t)
  </PRE>
<!--
  <P>
    Or if you actually want the abstraction:
  </P>

  <PRE>
	structure P = Package.Pack(structure X = A0 :> S
				   signature S = S where type t = X.t)
  </PRE>
-->
  <P>
    This bug will be corrected very shortly.
    Note that it does not compromise soundness, though.
  </P>


<?php footing() ?>
