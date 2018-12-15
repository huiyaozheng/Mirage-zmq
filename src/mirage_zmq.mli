type socket_type = REQ | REP | DEALER | ROUTER | PUB | XPUB | SUB | XSUB | PUSH | PULL | PAIR

(** Secuirty mechanism types; CURVE not implemented *)
type mechanism_type = NULL | PLAIN

(** A context is essentially a set of options shared by a group of socket *)
module Context : sig
    type t
    val create_context : unit -> t
end

(** Due to the characteristics of a unikernel, we need the network stack module to create TCP sockets *)
module Socket_tcp : functor (S : Mirage_stack_lwt.V4) -> sig 
    type t

    (** Create a socket from the given context, mechanism and type *)
    val create_socket : Context.t -> ?mechanism:mechanism_type -> socket_type -> t
    
    (** Set username and password for PLAIN client *)
    val set_plain_credentials : t -> string -> string -> unit
    
    (** Set password list for PLAIN server *)
    val set_plain_user_list : t -> (string * string) list -> unit
    
    (** Receive a msg from the underlying connections, according to the semantics of the socket type *)
    val recv : t -> string
    
    (** Send a msg to the underlying connections, according to the semantics of the socket type *)
    val send : t -> string -> unit
    
    (** Bind a local TCP port to the socket so the socket will accept incoming connections *)
    val bind : t -> int -> S.t -> unit
    
    (** Bind a connection to a remote TCP port to the socket *)
    val connect : t -> string -> int -> S.t -> unit
end

