(*
 * Standard ML infix resolution
 *
 * Definition, section 2.6
 *)


signature INFIX =
  sig

    (* Import *)

    structure Grammar: GRAMMAR_CORE

    type Exp     = Grammar.Exp
    type Pat     = Grammar.Pat

    type VId     = BasicObjects_Core.VId


    structure VIdFinMap: ORD_MAP where type Key.ord_key = VId


    (* Modifying fixity status *)

    datatype Assoc = LEFT | RIGHT

    type InfStatus = Assoc * int
    type InfEnv    = InfStatus VIdFinMap.map	(* "J" *)

    val empty:		InfEnv
    val assign:		InfEnv * VId list * InfStatus -> InfEnv
    val cancel:		InfEnv * VId list -> InfEnv

    (* Resolving phrases containing infixed identifiers *)

    val exp:	InfEnv -> Exp -> Exp
    val pat:	InfEnv -> Pat -> Pat

  end
