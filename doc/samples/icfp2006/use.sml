import "store"
import "decoder"
import "disassemble"
import "machine"

val s=Store.init"codex.umz"
val m=Machine.machine s

fun repeat 0 f x = ()
  | repeat n f x = (f x; repeat (n-1) f x)
