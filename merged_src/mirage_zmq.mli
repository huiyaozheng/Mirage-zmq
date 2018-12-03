type socket_type = REQ | REP | DEALER | ROUTER

type mechanism_type = NULL | PLAIN

module Context : sig
    type t
    val create_context : unit -> t
end

module type Socket = sig
    type t
    val create_socket : Context.t -> socket_type -> t
end

module Socket_tcp : functor (S : Mirage_stack_lwt.V4) -> sig 
    include Socket
    val bind : t -> int -> S.t -> unit S.io
end

module type Security_Mechanism = sig
    type t
    val name : string
end


