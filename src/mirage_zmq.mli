type socket_type = REQ | REP | DEALER | ROUTER | PUB | XPUB | SUB | XSUB | PUSH | PULL | PAIR

(** Secuirty mechanism types; CURVE not implemented *)
type mechanism_type = NULL | PLAIN

type message_component = Data of string | Identity of string

type message = message_component list

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

    (** Set identity string of a socket if applicable *)
    val set_identity : t -> string -> unit

    (** Set the maximum capacity of the incoming queue in terms of messages *)
    val set_incoming_queue_size: t -> int -> unit

    (** Set the maximum capacity of the outgoing queue in terms of messages *)
    val set_outgoing_queue_size: t -> int -> unit

    (** Add a subscription filter to SUB/XSUB socket *)
    val subscribe : t -> string -> unit

    (** Remove a subscription filter from SUB/XSUB socket *)
    val unsubscribe : t -> string -> unit
    
    (** Receive a msg from the underlying connections, according to the semantics of the socket type *)
    val recv : t -> message Lwt.t
    
    (** Send a msg to the underlying connections, according to the semantics of the socket type *)
    val send : t -> message -> unit
    
    (** Bind a local TCP port to the socket so the socket will accept incoming connections *)
    val bind : t -> int -> S.t -> unit
    
    (** Bind a connection to a remote TCP port to the socket *)
    val connect : t -> string -> int -> S.t -> unit Lwt.t
end

