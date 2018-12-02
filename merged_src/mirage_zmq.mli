type socket_type = REQ | REP | DEALER | ROUTER

type context

type socket

module type Socket = sig
    type t
    val name : string
end

module type Connection = sig
    type t
end

module type Security_Mechanism = sig
    type t
    val name : string
end

val new_context : unit -> context

val create_socket : context -> socket_type -> socket


