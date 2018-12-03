type socket_type = REQ | REP | DEALER | ROUTER

type mechanism_type = NULL | PLAIN

module Context : sig
    type t
    val create_context : unit -> t
end

module Socket : sig
    type t
    val create_socket : Context.t -> socket_type -> t
end

module type Socket = sig
    val name : string
end

module type Connection = sig
    type t
end

module type Security_Mechanism = sig
    type t
    val name : string
end


