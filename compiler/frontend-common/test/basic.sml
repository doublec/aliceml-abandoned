infix 4 +
infixr 5 ::

datatype a = A | B of {}
datatype 'a option = NONE | SOME of 'a

datatype t = T of t
datatype 'a u = U of 'a u

datatype 'a list = nil | :: of 'a * 'a list
datatype list' = datatype list

type v = t option u
type w = {} list

datatype 'a ext
constructor C of 'a * 'a option : 'a ext
constructor C' = C

exception E
exception E' = E

val x = 8

val cons = op::

val l = 0::nil

fun id x = x
val id = id

fun loop() = loop()
val nongen1 = loop()

val A = A
val B _ = B()
val C c = C(0, SOME 1)
val NONE = NONE
val SOME _ = SOME()
val T _ = raise E
val U _ = loop()
val nil = nil
val _::_ = nil

fun a + b = a + b
val op+ = op+
val nongen2 = 1 + 2

fun pair x y = (x,y)
val nongen3 = pair nil nil

fun length nil    = 0
  | length(x::xs) = 1 + length xs
