(*
Compiler.Symbol.symbolToString symbol
Compiler.StaticEnv.app (fn(symbol,_) => ()) staticEnv
Compiler.Environment.catalogEnv staticEnv
#get Compiler.EnvRef.topLevel ()
Compiler.EnvRef.combined ()
*)

fun printnl s = print(s^"\n")

val prenv = List.app (printnl o Compiler.Symbol.symbolToString) o Compiler.Environment.catalogEnv o #static o Compiler.CoerceEnv.b2e o Compiler.EnvRef.combined
