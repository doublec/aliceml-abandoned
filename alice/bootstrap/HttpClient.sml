(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2003
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

(* Dummy replacement for bootstrapping *)

structure Http =
    struct
	type request = {}
	type response = {statusCode: int, body: string}
    end

signature HTTP_CLIENT =
    sig
	type document = {contentType: string, body: string}

	exception Authority

	val request: Url.t * Http.request -> Http.response  (* Authority *)
	val get: Url.t -> Http.response                     (* Authority *)
	val post: Url.t * document -> Http.response         (* Authority *)
    end

structure HttpClient :> HTTP_CLIENT =
    struct
	type document = {contentType: string, body: string}

	exception Authority

	fun request (_, _) = raise Authority
	fun get _ = raise Authority
	fun post (_, _) = raise Authority
    end
