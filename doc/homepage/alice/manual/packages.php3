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
<TT><I>longstrid</I></TT>. The module must match the given
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

<P>The package can also be unpacked using a more general signature:</P>

<PRE class=code>
signature WORD_FROM_INT = sig type word val fromInt : int -> word end
structure WordFromInt = unpack p : WORD_FROM_INT</PRE>

<P>The full module subtyping rules apply to the unpack type check, so that
unpacking is quite robust with respect to extensions to interfaces of modules
packed elsewhere. In particular, arbitrary items may be added.</P>



<P>The package signature of <TT>p</TT> is <TT>WORD</TT>. That signature does not
specify anything about the identity of the contained <TT>word</TT> type.
Consequently

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
