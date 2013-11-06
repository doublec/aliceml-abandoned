(* (1) elaborates *)

signature S1 =
  sig
    datatype u = C of {} * {}
  end

structure S1 : S1 =
  struct
    datatype u = C of {} * {}
  end

(* (2) elaborates *)

signature S2 =
  sig
    type t = {} * {}
    datatype u = C of t
  end

structure S2 : S2 =
  struct
    type t = {} * {}
    datatype u = C of t
  end

(*
(* (3) does not elaborate *)

signature S3 =
  sig
    datatype u = C of {} * {}
  end

structure S3 : S3 =
  struct
    type t = {} * {}
    datatype u = C of t
  end

(* (4) does not elaborate *)

signature S4 =
  sig
    type t = {} * {}
    datatype u = C of t
  end

structure S4 : S4 =
  struct
    type t = {} * {}
    datatype u = C of {} * {}
  end
*)
