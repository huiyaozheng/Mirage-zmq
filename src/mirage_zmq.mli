(*
 * Copyright 2018-2019 Huiyao Zheng <huiyaozheng@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
exception No_Available_Peers

(** Raised when the function calls are invalid as defined by the RFCs, e.g. calling recv before send on a REQ socket. *)
exception Incorrect_use_of_API of string

(** Raised when the connection that is the target of send/source of recv unexpectedly closes. Catch this exception to re-try the current operation on another connection if available. *)
exception Connection_closed

(** Each ZMTP socket type has a corresponding type here. *)
type socket_type =
  | REQ
  | REP
  | DEALER
  | ROUTER
  | PUB
  | XPUB
  | SUB
  | XSUB
  | PUSH
  | PULL
  | PAIR

(** NULL and PLAIN security mechanisms are implemented in Mirage-zmq. *)
type mechanism_type = NULL | PLAIN

(** All socket types, except ROUTER, send and receive Data. ROUTER sends and receives Identity_and_data. *)
type message_type = Data of string | Identity_and_data of string * string

(** A context contains a set of default options (queue size). New sockets created in a context inherits the default options. *)
module Context : sig
  type t

  val create_context : unit -> t
  (** Create a new context with default queue sizes. *)

  val set_default_queue_size : t -> int -> unit
  (** Set the default queue size for this context. The queue size is measured in the number of messages in the queue. *)

  val get_default_queue_size : t -> int
  (** Get the default queue size for this context. *)
end

(** Due to the characteristics of a unikernel, we need the network stack module to create TCP sockets *)
module Socket_tcp (S : Mirage_stack_lwt.V4) : sig
  type t

  val create_socket :
    Context.t -> ?mechanism:mechanism_type -> socket_type -> t
  (** Create a socket in the given context, mechanism and type *)

  val set_plain_credentials : t -> string -> string -> unit
  (** Set user name and password for a socket of PLAIN mechanism. Call this function before connect or bind. *)

  val set_plain_user_list : t -> (string * string) list -> unit
  (** Set the admissible password list for a socket of PLAIN mechanism. Call this function before connect or bind. *)

  val set_identity : t -> string -> unit
  (** Set the IDENTITY property of a socket. Call this function before connect or bind. *)

  val set_incoming_queue_size : t -> int -> unit
  (** Set the maximum capacity (size) of the incoming queue in terms of the number of messages. *)

  val set_outgoing_queue_size : t -> int -> unit
  (** Set the maximum capacity (size) of the outgoing queue in terms of the number of messages. *)

  val subscribe : t -> string -> unit
  (** Add a subscription topic to SUB/XSUB socket. *)

  val unsubscribe : t -> string -> unit
  (** Remove a subscription topic from SUB/XSUB socket *)

  val recv : t -> message_type Lwt.t
  (** Receive a message from the socket, according to the semantics of the socket type. The returned promise is not resolved until a message is available. *)

  val send : t -> message_type -> unit Lwt.t
  (** Send a message to the connected peer(s), according to the semantics of the socket type. The returned promise is not resolved until the message enters the outgoing queue(s). *)

  val send_blocking : t -> message_type -> unit Lwt.t
  (** Send a message to the connected peer(s). The returned promise is not resolved until the message has been sent by the TCP connection. *)

  val bind : t -> int -> S.t -> unit
  (** Bind the socket to a local TCP port, so the socket will accept incoming connections. *)

  val connect : t -> string -> int -> S.t -> unit Lwt.t
  (** Connect the socket to a remote IP address and port. *)
end
