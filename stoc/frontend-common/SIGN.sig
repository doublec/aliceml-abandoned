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

    type id    = lab * int * stamp

    type ('inf,'sort) sign			(* [sigma,s] *)
    type ('inf,'sort) t = ('inf,'sort) sign

  (* Substitutions and realisations *)

    type subst = Path.subst

    type val_rea      = path PathMap.t
    type typ_rea      = typ  PathMap.t
    type mod_rea      = path PathMap.t
    type 'inf inf_rea = 'inf PathMap.t
    type 'inf rea     = val_rea * typ_rea * mod_rea * 'inf inf_rea

  (* Construction *)

    val empty :		unit -> ('a,'b) sign
    val extendVal :	('a,'b) sign * lab * typ * bool * path option -> id
    val extendTyp :	('a,'b) sign * lab * kind * typ option -> id
    val extendMod :	('a,'b) sign * lab * 'a  * path option -> id
    val extendInf :	('a,'b) sign * lab * 'b  *  'a  option -> id

  (* Lookup *)

    val lookupVal :	('a,'b) sign * lab -> typ
    val lookupTyp :	('a,'b) sign * lab -> typ
    val lookupMod :	('a,'b) sign * lab -> 'a
    val lookupInf :	('a,'b) sign * lab -> 'a

    val lookupVal' :	('a,'b) sign * lab * int -> typ
    val lookupTyp' :	('a,'b) sign * lab * int -> typ
    val lookupMod' :	('a,'b) sign * lab * int -> 'a
    val lookupInf' :	('a,'b) sign * lab * int -> 'a

  (* Instantiation etc. *)

    val substitute :	(subst * 'a -> unit) * (subst * 'b -> unit) ->
			 subst * ('a,'b) sign -> unit
    val strengthen :	(subst * path * 'a -> unit) * (subst * 'a -> unit) *
			(subst * 'b -> unit) * ('b * path -> 'a) ->
			 subst * path * ('a,'b) sign -> unit
    val instantiate :	(subst * 'a -> 'a) * (subst * 'b -> 'b) ->
			 subst * ('a,'b) sign -> ('a,'b) sign

  end
