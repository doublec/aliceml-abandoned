(* To break the recursion between Sign and Inf we have parameterised over
   the types inf and sort. *)

signature SIGN =
  sig

  (* Types *)

    type lab   = Lab.t
    type name  = Name.t
    type stamp = Stamp.t
    type path  = Path.t
    type typ   = Type.t
    type kind  = Type.kind

    datatype val_sort = VALUE | CONSTRUCTOR	(* [w] *)
    datatype typ_sort = datatype Type.sort	(* [w] *)

    type ('inf,'kind) sign			(* [sigma,s] *)
    type ('inf,'kind) t = ('inf,'kind) sign

  (* Realisations *)

    type rea		 = path PathMap.t

    type val_rea	 = path PathMap.t
    type typ_rea	 = typ  PathMap.t
    type ('i,'k) mod_rea = ('i,'k) sign PathMap.t
    type ('i,'k) inf_rea = 'i PathMap.t
    type ('i,'k) rea'	 = val_rea * typ_rea * ('i,'k) mod_rea * ('i,'k) inf_rea

  (* Construction *)

    val empty :		unit -> ('a,'b) sign

    val newVal :	('a,'b) sign * lab -> path
    val newTyp :	('a,'b) sign * lab -> path
    val newMod :	('a,'b) sign * lab -> path
    val newInf :	('a,'b) sign * lab -> path

    val extendVal :	('a,'b) sign * path * typ  * val_sort * path option
								 -> path
    val extendTyp :	('a,'b) sign * path * kind * typ_sort * typ option
								 -> path
    val extendMod :	('a,'b) sign * path *  'a  * path option -> path
    val extendInf :	('a,'b) sign * path *  'b  *  'a  option -> path

  (* Lookup *)

    val lookupVal :	('a,'b) sign * lab -> typ
    val lookupTyp :	('a,'b) sign * lab -> typ
    val lookupMod :	('a,'b) sign * lab -> 'a
    val lookupInf :	('b * path -> 'a) -> ('a,'b) sign * lab -> 'a

    val lookupVal' :	('a,'b) sign * lab * int -> typ
    val lookupTyp' :	('a,'b) sign * lab * int -> typ
    val lookupMod' :	('a,'b) sign * lab * int -> 'a
    val lookupInf' :	('b * path -> 'a) -> ('a,'b) sign * lab * int -> 'a

  (* Realisations and cloning *)

    val realise :	(rea * 'a -> unit) * (rea * 'b -> unit) ->
			 rea * ('a,'b) sign -> unit
    val clone :		(rea * 'a -> 'a) * (rea * 'b -> 'b) ->
			 rea * ('a,'b) sign -> ('a,'b) sign
    val strengthen :	('b * path -> 'a) -> path * ('a,'b) sign -> unit

  end
