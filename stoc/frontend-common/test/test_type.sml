Compiler.Control.Print.printDepth:=100;
CM.make' "~/src/stockhausen/stoc/frontend-common/sources.cm";

val E = Env.new();
val id = AbstractGrammar.Id(Source.nowhere, x, Name.InId);

val a = Type.var Type.STAR;
val y = Stamp.new();
Env.insertVar(E,y,(id,a));

val x = Stamp.new();
val k = Type.ARROW(Type.STAR,Type.STAR);
val t1 = Type.unknown k;
Env.insertTyp(E,x,(id,t1));

val ta = Type.inVar a;
val t2 = Type.inApp(t1,ta);


val ka = Type.kind ta;
val k1 = Type.kind t1;
val k2 = Type.kind t2;
