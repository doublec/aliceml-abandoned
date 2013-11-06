signature S = sig type t end


val xManifest = 0
signature MANIFEST_VAL = sig val x = xManifest end


signature P(X: S) = sig structure Y = X end

signature F = fct (X: S) -> S where type t = X.t

structure F : F = fct (X: S) => X


fun package() = pack F

structure F' = unpack package() : F
