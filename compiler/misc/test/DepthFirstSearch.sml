structure StringMap = MakeHashImpMap(StringHashKey)
structure DepthFirstSearch =
    MakeDepthFirstSearch(structure Key = StringHashKey
			 structure Map = StringMap)

val map: string list StringMap.t = StringMap.new ()

val _ = StringMap.insert (map, "a", ["b", "d"])
val _ = StringMap.insert (map, "b", ["c"])
val _ = StringMap.insert (map, "c", ["g"])
val _ = StringMap.insert (map, "d", ["e", "c"])
val _ = StringMap.insert (map, "e", ["f", "g"])
val _ = StringMap.insert (map, "f", ["d"])
val _ = StringMap.insert (map, "g", [])

val sss = DepthFirstSearch.search map
val _ = List.app (fn ss => TextIO.print (List.foldl op^ "\n" ss)) sss
