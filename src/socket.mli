type t

type socket = REQ | REP | DEALER | ROUTER

val create : Context.t -> socket -> t