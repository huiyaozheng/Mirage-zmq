type socket_type = REQ | REP | DEALER | ROUTER | PUB | XPUB | SUB | XSUB | PUSH | PULL | PAIR

type mechanism_type = NULL | PLAIN

module Context : sig
    type t
    val create_context : unit -> t
end

module type Socket = sig
    type t
    val create_socket : Context.t -> ?mechanism:mechanism_type -> socket_type -> t
    (* Set the username and password for PLAIN mechanism *)
    val set_plain_credentials : t -> string -> string -> t
    (* Set the admittable users for PLAIN mechanism *)
    val set_plain_user_list : t -> (string * string) list -> t
    val recv : t -> string
    val send : t -> string -> unit
end

module Socket_tcp : functor (S : Mirage_stack_lwt.V4) -> sig 
    include Socket
    val bind : t -> int -> S.t -> unit 
    val connect : t -> string -> int -> S.t -> unit S.io

end

