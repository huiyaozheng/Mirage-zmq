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

exception Incorrect_use_of_API of string

exception Connection_closed
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

(** Secuirty mechanism types; CURVE not implemented *)
type mechanism_type = NULL | PLAIN

type message_type = Data of string | Identity_and_data of string * string

(** A context is essentially a set of options shared by a group of socket *)
module Context : sig
  type t

  val create_context : unit -> t

  val set_default_queue_size : t -> int -> unit
  (** Set the default queue size for this context *)

  val get_default_queue_size : t -> int
  (** Get the default queue size for this context *)
end

(** Due to the characteristics of a unikernel, we need the network stack module to create TCP sockets *)
module Socket_tcp (S : Mirage_stack_lwt.V4) : sig
  type t

  val create_socket :
    Context.t -> ?mechanism:mechanism_type -> socket_type -> t
  (** Create a socket from the given context, mechanism and type *)

  val set_plain_credentials : t -> string -> string -> unit
  (** Set username and password for PLAIN client *)

  val set_plain_user_list : t -> (string * string) list -> unit
  (** Set password list for PLAIN server *)

  val set_identity : t -> string -> unit
  (** Set identity string of a socket if applicable *)

  val set_incoming_queue_size : t -> int -> unit
  (** Set the maximum capacity of the incoming queue in terms of messages *)

  val set_outgoing_queue_size : t -> int -> unit
  (** Set the maximum capacity of the outgoing queue in terms of messages *)

  val subscribe : t -> string -> unit
  (** Add a subscription filter to SUB/XSUB socket *)

  val unsubscribe : t -> string -> unit
  (** Remove a subscription filter from SUB/XSUB socket *)

  val recv : t -> message_type Lwt.t
  (** Receive a msg from the underlying connections, according to the semantics of the socket type *)

  val send : t -> message_type -> unit Lwt.t
  (** Send a msg to the underlying connections, according to the semantics of the socket type *)

  val send_blocking : t -> message_type -> unit Lwt.t
  (** Send a msg to the underlying connections. It blocks until a peer is available *)

  val bind : t -> int -> S.t -> unit
  (** Bind a local TCP port to the socket so the socket will accept incoming connections *)

  val connect : t -> string -> int -> S.t -> unit Lwt.t
  (** Bind a connection to a remote TCP port to the socket *)
end
