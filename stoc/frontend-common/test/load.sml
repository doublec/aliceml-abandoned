__prebound P

type string = P.string

__primitive functor Load(X: sig val url: string signature S end) : X.S = "Load"
__primitive functor Save(val url: string signature S structure M: S) : any = "Save"

signature S1 = sig type t end

structure M1 = Load(val url = "http://www.stockhausen.org/test" signature S = S1)
structure D1 = Save(val url = "/tmp/test" signature S = S1 structure M = M1:>S1)
