structure BoolVector :> MONO_VECTOR where type elem = bool = 
    struct 
	open Vector 
	type elem = bool 
	type vector = bool vector 
    end

structure IntSet = RedBlackSetFn (type ord_key = int val compare = Int.compare)
    
    
structure IntMap = RedBlackMapFn (type ord_key = int val compare = Int.compare)
    
    
structure StringMap = RedBlackMapFn (type ord_key = string val compare = String.compare)
    

structure SetMap = RedBlackMapFn (type ord_key = IntSet.set val compare = IntSet.compare)
