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
open Lwt.Infix

let default_queue_size = 1000

exception No_Available_Peers

exception Not_Implemented

exception Should_Not_Reach

exception Socket_Name_Not_Recognised

exception Not_Able_To_Set_Credentials

exception Internal_Error of string

exception Incorrect_use_of_API of string

exception Connection_closed

exception Queue_overflow

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

(* CURVE not implemented *)
type mechanism_type = NULL | PLAIN

type message = Data of string | Identity_and_data of string * string

type socket_metadata = (string * string) list

type security_data =
  | Null
  | Plain_client of string * string
  | Plain_server of (string, string) Hashtbl.t

type connection_stage = GREETING | HANDSHAKE | TRAFFIC | CLOSED

type connection_fsm_data = Input_data of Bytes.t | End_of_connection

module Utils = struct
  (** Convert a series of big-endian bytes to int *)
  let rec network_order_to_int bytes =
    let length = Bytes.length bytes in
    if length = 1 then Char.code (Bytes.get bytes 0)
    else
      Char.code (Bytes.get bytes (length - 1))
      + (network_order_to_int (Bytes.sub bytes 0 (length - 1)) * 256)

  (** Convert a int to big-endian bytes of length n *)
  let int_to_network_order n length =
    Bytes.init length (fun i ->
        Char.chr ((n lsr (8 * (length - i - 1))) land 255) )

  (** Converts a byte buffer to printable string *)
  let buffer_to_string data =
    let content = ref [] in
    Bytes.iter (fun b -> content := Char.code b :: !content) data ;
    String.concat " "
      (List.map
         (fun x ->
           if (x >= 65 && x <= 90) || (x >= 97 && x <= 122) then
             String.make 1 (Char.chr x)
           else string_of_int x )
         (List.rev !content))
end

module Frame : sig
  type t

  val make_frame : Bytes.t -> if_more:bool -> if_command:bool -> t
  (** make_frame body ifMore ifCommand *)

  val to_bytes : t -> Bytes.t
  (** Convert a frame to raw bytes *)

  val list_of_bytes : Bytes.t -> t list * Bytes.t
  (** Construct a list of frames from raw bytes and returns any remaining fragment *)

  val get_if_more : t -> bool
  (** Get if_more flag from a frame *)

  val get_if_long : t -> bool
  (** Get if_long flag from a frame *)

  val get_if_command : t -> bool
  (** Get if_command flag from a frame *)

  val get_body : t -> Bytes.t
  (** Get body from a frame *)

  val is_delimiter_frame : t -> bool
  (** A helper function checking whether a frame is empty (delimiter) *)

  val delimiter_frame : t
  (** Make a delimiter frame*)

  val splice_message_frames : t list -> string
  (** A helper function that takes a list of message frames and returns the reconstructed message *)
end = struct
  type t =
    { flags: char
    ; (* Known issue: size is limited by max_int; may not be able to reach 2^63-1 *)
      size: int
    ; body: bytes }

  let size_to_bytes size if_long =
    if not if_long then Bytes.make 1 (Char.chr size)
    else Utils.int_to_network_order size 8

  let make_frame body ~if_more ~if_command =
    let f = ref 0 in
    let len = Bytes.length body in
    if if_more then f := !f + 1 ;
    if if_command then f := !f + 4 ;
    if len > 255 then f := !f + 2 ;
    {flags= Char.chr !f; size= len; body}

  let to_bytes t =
    Bytes.concat Bytes.empty
      [ Bytes.make 1 t.flags
      ; size_to_bytes t.size (Char.code t.flags land 2 = 2)
      ; t.body ]

  let rec list_of_bytes bytes =
    let total_length = Bytes.length bytes in
    if total_length < 2 then ([], bytes)
    else
      let flag = Char.code (Bytes.get bytes 0) in
      let if_long = flag land 2 = 2 in
      if if_long then
        if (* long-size *)
           total_length < 9 then ([], bytes)
        else
          let content_length =
            Utils.network_order_to_int (Bytes.sub bytes 1 8)
          in
          if total_length < content_length + 9 then ([], bytes)
          else if total_length > content_length + 9 then
            let list, remain =
              list_of_bytes
                (Bytes.sub bytes (content_length + 9)
                   (total_length - content_length - 9))
            in
            ( { flags= Char.chr flag
              ; size= content_length
              ; body= Bytes.sub bytes 9 content_length }
              :: list
            , remain )
          else
            ( [ { flags= Char.chr flag
                ; size= content_length
                ; body= Bytes.sub bytes 9 content_length } ]
            , Bytes.empty )
      else
        (* short-size *)
        let content_length = Char.code (Bytes.get bytes 1) in
        (* check length *)
        if total_length < content_length + 2 then ([], bytes)
        else if total_length > content_length + 2 then
          let list, remain =
            list_of_bytes
              (Bytes.sub bytes (content_length + 2)
                 (total_length - content_length - 2))
          in
          ( { flags= Char.chr flag
            ; size= content_length
            ; body= Bytes.sub bytes 2 content_length }
            :: list
          , remain )
        else
          ( [ { flags= Char.chr flag
              ; size= content_length
              ; body= Bytes.sub bytes 2 content_length } ]
          , Bytes.empty )

  let get_if_more t = Char.code t.flags land 1 = 1

  let get_if_long t = Char.code t.flags land 2 = 2

  let get_if_command t = Char.code t.flags land 4 = 4

  let get_body t = t.body

  let is_delimiter_frame t = get_if_more t && t.size = 0

  let delimiter_frame = {flags= Char.chr 1; size= 0; body= Bytes.empty}

  let splice_message_frames list =
    let rec splice_message_frames_accumu list s =
      match list with
      | [] ->
          s
      | hd :: tl -> (
        match tl with
        | [] ->
            if get_if_more hd then raise (Internal_Error "Missing frames")
            else
              splice_message_frames_accumu tl
                (s ^ Bytes.to_string (get_body hd))
        | _ ->
            if not (get_if_more hd) then
              raise (Internal_Error "Too many  frames")
            else
              splice_message_frames_accumu tl
                (s ^ Bytes.to_string (get_body hd)) )
    in
    splice_message_frames_accumu list ""
end

module Command : sig
  type t

  val to_frame : t -> Frame.t
  (** Convert a command to a frame *)

  val get_name : t -> string
  (** Get name of the command (without length byte) *)

  val get_data : t -> bytes
  (** Get data of the command *)

  val of_frame : Frame.t -> t
  (** Construct a command from the enclosing frame *)

  val make_command : string -> bytes -> t
  (** Construct a command from given name and data *)
end = struct
  type t = {name: string; data: bytes}

  let to_frame t =
    Frame.make_frame
      (Bytes.concat Bytes.empty
         [ Bytes.make 1 (Char.chr (String.length t.name))
         ; Bytes.of_string t.name
         ; t.data ])
      ~if_more:false ~if_command:true

  let get_name t = t.name

  let get_data t = t.data

  let of_frame frame =
    let data = Frame.get_body frame in
    let name_length = Char.code (Bytes.get data 0) in
    { name= Bytes.sub_string data 1 name_length
    ; data=
        Bytes.sub data (name_length + 1) (Bytes.length data - 1 - name_length)
    }

  let make_command command_name data = {name= command_name; data}
end

module Message : sig
  type t

  val of_string : ?if_long:bool -> ?if_more:bool -> string -> t
  (** Make a message from the string *)

  val list_of_string : string -> t list
  (** Make a list of messages from the string to send *)

  val to_frame : t -> Frame.t
  (** Convert a message to a frame *)
end = struct
  type t = {size: int; if_long: bool; if_more: bool; body: bytes}

  let of_string ?(if_long = false) ?(if_more = false) msg =
    let length = String.length msg in
    if length > 255 && not if_long then
      raise (Internal_Error "Must be long message")
    else {size= length; if_long; if_more; body= Bytes.of_string msg}

  let list_of_string msg =
    let length = String.length msg in
    (* Assume Sys.max_string_length < max_int *)
    if length > 1020 then
      (* Make a LONG message *)
      [of_string msg ~if_long:true ~if_more:false]
    else
      (* Make short messages *)
      let rec make msg list =
        let length = String.length msg in
        if length > 255 then
          make
            (String.sub msg 255 (length - 255))
            ( of_string (String.sub msg 0 255) ~if_long:false ~if_more:true
            :: list )
        else of_string msg ~if_long:false ~if_more:false :: list
      in
      List.rev (make msg [])

  let to_frame t = Frame.make_frame t.body ~if_more:t.if_more ~if_command:false
end

module Context : sig
  type t

  val create_context : unit -> t
  (** Create a new context *)

  val set_default_queue_size : t -> int -> unit
  (** Set the default queue size for this context *)

  val get_default_queue_size : t -> int
  (** Get the default queue size for this context *)

  val destroy_context : t -> unit
  (** Destroy all sockets initialised by the context. All connections will be closed *)
end = struct
  type t = {mutable default_queue_size: int}

  let create_context () = {default_queue_size}

  let set_default_queue_size context size = context.default_queue_size <- size

  let get_default_queue_size context = context.default_queue_size

  (* TODO close all connections of sockets in the context *)
  let destroy_context (t : t) = ()
end

module rec Socket : sig
  type t

  val socket_type_from_string : string -> socket_type

  val if_valid_socket_pair : socket_type -> socket_type -> bool

  val if_queue_size_limited : socket_type -> bool

  val if_has_incoming_queue : socket_type -> bool

  val if_has_outgoing_queue : socket_type -> bool

  val get_socket_type : t -> socket_type
  (** Get the type of the socket *)

  val get_metadata : t -> socket_metadata
  (** Get the metadata of the socket for handshake *)

  val get_mechanism : t -> mechanism_type
  (** Get the security mechanism of the socket *)

  val get_security_data : t -> security_data
  (** Get the security credentials of the socket *)

  val get_incoming_queue_size : t -> int
  (** Get the maximum capacity of the incoming queue *)

  val get_outgoing_queue_size : t -> int
  (** Get the maximum capacity of the outgoing queue *)

  val get_pair_connected : t -> bool
  (** Whether a PAIR is already connected to another PAIR *)

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
  (** Set the maximum capacity of the incoming queue *)

  val set_outgoing_queue_size : t -> int -> unit
  (** Set the maximum capacity of the outgoing queue *)

  val set_pair_connected : t -> bool -> unit
  (** Set PAIR's connection status *)

  val subscribe : t -> string -> unit

  val unsubscribe : t -> string -> unit

  val recv : t -> message Lwt.t
  (** Receive a msg from the underlying connections, according to the semantics of the socket type *)

  val send : t -> message -> unit Lwt.t
  (** Send a msg to the underlying connections, according to the semantics of the socket type *)

  val send_blocking : t -> message -> unit Lwt.t

  val add_connection : t -> Connection.t ref -> unit

  val initial_traffic_messages : t -> Frame.t list
  (** Get the messages to send at the beginning of a connection, e.g. subscriptions *)
end = struct
  type socket_states =
    | NONE
    | Rep of
        { if_received: bool
        ; last_received_connection_tag: string
        ; address_envelope: Frame.t list }
    | Req of {if_sent: bool; last_sent_connection_tag: string}
    | Dealer of {request_order_queue: string Queue.t}
    | Router
    | Pub
    | Sub of {subscriptions: string list}
    | Xpub
    | Xsub of {subscriptions: string list}
    | Push
    | Pull
    | Pair of {connected: bool}

  type t =
    { socket_type: socket_type
    ; mutable metadata: socket_metadata
    ; security_mechanism: mechanism_type
    ; mutable security_info: security_data
    ; mutable connections: Connection.t ref Queue.t
    ; mutable socket_states: socket_states
    ; mutable incoming_queue_size: int option
    ; mutable outgoing_queue_size: int option }

  (* Start of helper functions *)

  (** Returns the socket type from string *)
  let socket_type_from_string = function
    | "REQ" ->
        REQ
    | "REP" ->
        REP
    | "DEALER" ->
        DEALER
    | "ROUTER" ->
        ROUTER
    | "PUB" ->
        PUB
    | "XPUB" ->
        XPUB
    | "SUB" ->
        SUB
    | "XSUB" ->
        XSUB
    | "PUSH" ->
        PUSH
    | "PULL" ->
        PULL
    | "PAIR" ->
        PAIR
    | _ ->
        raise Socket_Name_Not_Recognised

  (** Checks if the pair is valid as specified by 23/ZMTP *)
  let if_valid_socket_pair a b =
    match (a, b) with
    | REQ, REP
    | REQ, ROUTER
    | REP, REQ
    | REP, DEALER
    | DEALER, REP
    | DEALER, DEALER
    | DEALER, ROUTER
    | ROUTER, REQ
    | ROUTER, DEALER
    | ROUTER, ROUTER
    | PUB, SUB
    | PUB, XSUB
    | XPUB, SUB
    | XPUB, XSUB
    | SUB, PUB
    | SUB, XPUB
    | XSUB, PUB
    | XSUB, XPUB
    | PUSH, PULL
    | PULL, PUSH
    | PAIR, PAIR ->
        true
    | _ ->
        false

  (** Whether the socket type has connections with limited-size queues *)
  let if_queue_size_limited socket =
    match socket with
    | REP | REQ ->
        false
    | DEALER | ROUTER | PUB | SUB | XPUB | XSUB | PUSH | PULL | PAIR ->
        true

  (** Whether the socket type has an outgoing queue *)
  let if_has_outgoing_queue socket =
    match socket with
    | REP | REQ | DEALER | ROUTER | PUB | SUB | XPUB | XSUB | PUSH | PAIR ->
        true
    | PULL ->
        false

  (** Whether the socket type has an incoming queue *)
  let if_has_incoming_queue socket =
    match socket with
    | REP | REQ | DEALER | ROUTER | PUB | SUB | XPUB | XSUB | PULL | PAIR ->
        true
    | PUSH ->
        false

  (* If success, the connection at the front of the queue is available and returns it.
    Otherwise, return None *)
  let rec find_available_connection connections =
    if Queue.is_empty connections then None
    else
      let buffer_queue = Queue.create () in
      let rec rotate () =
        if Queue.is_empty connections then (
          Queue.transfer buffer_queue connections ;
          None )
        else
          let head = Queue.peek connections in
          if
            Connection.get_stage !head <> TRAFFIC
            || Connection.if_send_queue_full !head
          then (
            Queue.push (Queue.pop connections) buffer_queue ;
            rotate () )
          else (
            Queue.transfer buffer_queue connections ;
            Some !head )
      in
      rotate ()

  (** Put connection at the end of the list or remove the connection from the list *)
  let rotate list connection if_remove_head =
    let rec rotate_accumu list accumu =
      match list with
      | [] ->
          if not if_remove_head then connection :: accumu else accumu
      | hd :: tl ->
          if Connection.get_tag !hd = Connection.get_tag !connection then
            rotate_accumu tl accumu
          else rotate_accumu tl (hd :: accumu)
    in
    List.rev (rotate_accumu list [])

  let rotate connections if_drop_head =
    if if_drop_head then ignore (Queue.pop connections)
    else Queue.push (Queue.pop connections) connections

  (* connections is a queue of Connection.t ref. 
    This functions returns the Connection.t with the compare function if found *)
  let find_connection connections comp =
    Queue.fold
      (fun accum connection_ref ->
        match accum with
        | Some _ ->
            accum
        | None ->
            let connection = !connection_ref in
            if comp connection then Some connection else accum )
      None connections

  (* Rotate a queue until the front connection has non-empty incoming buffer.
    Will rotate indefinitely until return, unless queue is empty at start*)
  let rec find_connection_with_incoming_buffer connections =
    if Queue.is_empty connections then Lwt.return None
    else
      let head = !(Queue.peek connections) in
      if Connection.get_stage head = TRAFFIC then
        let buffer = Connection.get_read_buffer head in
        if Queue.is_empty buffer then (
          rotate connections false ;
          Lwt.pause ()
          >>= fun () -> find_connection_with_incoming_buffer connections )
        else Lwt.return (Some head)
      else if Connection.get_stage head = CLOSED then (
        rotate connections true ;
        find_connection_with_incoming_buffer connections )
      else (
        rotate connections false ;
        Lwt.pause ()
        >>= fun () -> find_connection_with_incoming_buffer connections )

  (* Get the next list of frames containing a complete message/command from the read buffer.
  Return None if the stream is closed.
  Block if message not complete
  *)
  let get_frame_list connection =
    let rec get_reverse_frame_list_accumu list =
      try
        Queue.pop (Connection.get_read_buffer connection)
        |> function
        | None ->
            (* Stream closed *) Lwt.return None
        | Some next_frame ->
            if Frame.get_if_more next_frame then
              get_reverse_frame_list_accumu (next_frame :: list)
            else Lwt.return (Some (next_frame :: list))
      with Queue.Empty ->
        (* Wait for more frames to come *)
        Lwt.pause () >>= fun () -> get_reverse_frame_list_accumu list
    in
    get_reverse_frame_list_accumu []
    >>= function
    | None ->
        Lwt.return_none
    | Some frames ->
        Lwt.return_some (List.rev frames)

  (* receive from the first available connection in the queue and rotate the queue once after receving *)
  let rec receive_and_rotate connections =
    if Queue.is_empty connections then
      Lwt.pause () >>= fun () -> receive_and_rotate connections
    else
      find_connection_with_incoming_buffer connections
      >>= function
      | None ->
          Lwt.pause () >>= fun () -> receive_and_rotate connections
      | Some connection -> (
          (* Reconstruct message from the connection *)
          get_frame_list connection
          >>= function
          | None ->
              Connection.close connection ;
              rotate connections true ;
              Lwt.pause () >>= fun () -> receive_and_rotate connections
          | Some frames ->
              rotate connections false ;
              Lwt.return (Data (Frame.splice_message_frames frames)) )

  (* Get the address envelope from the read buffer, stop when the empty delimiter is encountered and discard the delimiter.connections
  Return None if stream closed
  Block if envelope not complete yet *)
  let get_address_envelope connection =
    let rec get_reverse_frame_list_accumu list =
      try
        Queue.pop (Connection.get_read_buffer connection)
        |> function
        | None ->
            (* Stream closed *) Lwt.return None
        | Some next_frame ->
            if Frame.is_delimiter_frame next_frame then Lwt.return (Some list)
            else if Frame.get_if_more next_frame then
              get_reverse_frame_list_accumu (next_frame :: list)
            else Lwt.return (Some (next_frame :: list))
      with Queue.Empty ->
        Lwt.pause () >>= fun () -> get_reverse_frame_list_accumu list
    in
    get_reverse_frame_list_accumu []
    >>= function
    | None ->
        Lwt.return_none
    | Some frames ->
        Lwt.return_some (List.rev frames)

  (** Check whether the content matches with any entry in the subscriptions *)
  let match_subscriptions content subscriptions =
    let match_subscription subscription =
      let content_length = String.length content in
      let subscription_length = String.length subscription in
      if subscription_length = 0 then true
      else if content_length >= subscription_length then
        String.sub content 0 subscription_length = subscription
      else false
    in
    if subscriptions = [] then false
    else
      List.fold_left
        (fun flag x -> flag || match_subscription x)
        false subscriptions

  (* Broadcast a message to all connections that satisfy predicate if_send_to in the queue *)
  let broadcast connections msg if_send_to =
    let frames_to_send =
      List.map (fun x -> Message.to_frame x) (Message.list_of_string msg)
    in
    let publish connection =
      if if_send_to connection then
        try Connection.send connection frames_to_send
        with (* Drop message when queue full *)
        | Queue_overflow ->
          Lwt.return_unit
      else Lwt.return_unit
    in
    Queue.iter
      (fun x ->
        if Connection.get_stage !x = TRAFFIC then
          Lwt.async (fun () -> publish !x)
        else () )
      connections

  let subscription_frame content =
    Frame.make_frame
      (Bytes.cat (Bytes.make 1 (Char.chr 1)) (Bytes.of_string content))
      ~if_more:false ~if_command:false

  let unsubscription_frame content =
    Frame.make_frame
      (Bytes.cat (Bytes.make 1 (Char.chr 0)) (Bytes.of_string content))
      ~if_more:false ~if_command:false

  let rec send_message_to_all_active_connections connections frame =
    Queue.iter
      (fun x ->
        if Connection.get_stage !x = TRAFFIC then
          Lwt.async (fun () -> Connection.send !x [frame]) )
      connections

  (* End of helper functions *)

  let get_socket_type t = t.socket_type

  let get_metadata t = t.metadata

  let get_mechanism t = t.security_mechanism

  let get_security_data t = t.security_info

  let get_incoming_queue_size t =
    match t.incoming_queue_size with
    | Some x ->
        x
    | None ->
        raise (Internal_Error "Incoming queue size is not defined")

  let get_outgoing_queue_size t =
    match t.outgoing_queue_size with
    | Some x ->
        x
    | None ->
        raise (Internal_Error "Outgoing queue size is not defined")

  let get_pair_connected t =
    match t.socket_states with
    | Pair {connected} ->
        connected
    | _ ->
        raise
          (Incorrect_use_of_API
             "Cannot call this function on a socket other than PAIR!")

  let create_socket context ?(mechanism = NULL) socket_type =
    match socket_type with
    | REP ->
        { socket_type
        ; metadata= [("Socket-Type", "REP")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states=
            Rep
              { if_received= false
              ; last_received_connection_tag= ""
              ; address_envelope= [] }
        ; incoming_queue_size= None
        ; outgoing_queue_size= None }
    | REQ ->
        { socket_type
        ; metadata= [("Socket-Type", "REQ")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Req {if_sent= false; last_sent_connection_tag= ""}
        ; incoming_queue_size= None
        ; outgoing_queue_size= None }
    | DEALER ->
        { socket_type
        ; metadata= [("Socket-Type", "DEALER"); ("Identity", "")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Dealer {request_order_queue= Queue.create ()}
        ; incoming_queue_size= Some (Context.get_default_queue_size context)
        ; outgoing_queue_size= Some (Context.get_default_queue_size context) }
    | ROUTER ->
        { socket_type
        ; metadata= [("Socket-Type", "ROUTER")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Router
        ; incoming_queue_size= Some (Context.get_default_queue_size context)
        ; outgoing_queue_size= Some (Context.get_default_queue_size context) }
    | PUB ->
        { socket_type
        ; metadata= [("Socket-Type", "PUB")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Pub
        ; incoming_queue_size= Some (Context.get_default_queue_size context)
        ; outgoing_queue_size= Some (Context.get_default_queue_size context) }
    | XPUB ->
        { socket_type
        ; metadata= [("Socket-Type", "XPUB")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Xpub
        ; incoming_queue_size= Some (Context.get_default_queue_size context)
        ; outgoing_queue_size= Some (Context.get_default_queue_size context) }
    | SUB ->
        { socket_type
        ; metadata= [("Socket-Type", "SUB")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Sub {subscriptions= []}
        ; incoming_queue_size=
            Some (Context.get_default_queue_size context)
            (* Need an outgoing queue to send subscriptions *)
        ; outgoing_queue_size= Some (Context.get_default_queue_size context) }
    | XSUB ->
        { socket_type
        ; metadata= [("Socket-Type", "XSUB")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Xsub {subscriptions= []}
        ; incoming_queue_size= Some (Context.get_default_queue_size context)
        ; outgoing_queue_size= Some (Context.get_default_queue_size context) }
    | PUSH ->
        { socket_type
        ; metadata= [("Socket-Type", "PUSH")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Push
        ; incoming_queue_size= None
        ; outgoing_queue_size= Some (Context.get_default_queue_size context) }
    | PULL ->
        { socket_type
        ; metadata= [("Socket-Type", "PULL")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Pull
        ; incoming_queue_size= Some (Context.get_default_queue_size context)
        ; outgoing_queue_size= None }
    | PAIR ->
        { socket_type
        ; metadata= [("Socket-Type", "PAIR")]
        ; security_mechanism= mechanism
        ; security_info= Null
        ; connections= Queue.create ()
        ; socket_states= Pair {connected= false}
        ; incoming_queue_size= Some (Context.get_default_queue_size context)
        ; outgoing_queue_size= Some (Context.get_default_queue_size context) }

  let set_plain_credentials t name password =
    if t.security_mechanism = PLAIN then
      t.security_info <- Plain_client (name, password)
    else raise Not_Able_To_Set_Credentials

  let set_plain_user_list t list =
    if t.security_mechanism = PLAIN then (
      let hashtable = Hashtbl.create (List.length list) in
      List.iter
        (fun (username, password) -> Hashtbl.add hashtable username password)
        list ;
      t.security_info <- Plain_server hashtable )
    else raise Not_Able_To_Set_Credentials

  let set_identity t identity =
    let set (name, value) =
      if name = "Identity" then (name, identity) else (name, value)
    in
    if
      List.fold_left
        (fun b (name, _) -> b || name = "Identity")
        false t.metadata
    then t.metadata <- List.map set t.metadata
    else t.metadata <- t.metadata @ [("Identity", identity)]

  let set_incoming_queue_size t size = t.incoming_queue_size <- Some size

  let set_outgoing_queue_size t size = t.outgoing_queue_size <- Some size

  let set_pair_connected t status =
    match t.socket_type with
    | PAIR ->
        t.socket_states <- Pair {connected= status}
    | _ ->
        raise
          (Incorrect_use_of_API "This state can only be set for PAIR socket!")

  let subscribe t (subscription : string) =
    match t.socket_type with
    | SUB | XSUB -> (
      match t.socket_states with
      | Sub {subscriptions} ->
          t.socket_states <- Sub {subscriptions= subscription :: subscriptions} ;
          send_message_to_all_active_connections t.connections
            (subscription_frame subscription)
      | Xsub {subscriptions} ->
          t.socket_states <- Xsub {subscriptions= subscription :: subscriptions} ;
          send_message_to_all_active_connections t.connections
            (subscription_frame subscription)
      | _ ->
          raise Should_Not_Reach )
    | _ ->
        raise
          (Incorrect_use_of_API "This socket does not support subscription!")

  let unsubscribe t subscription =
    let rec check_and_remove subscriptions =
      match subscriptions with
      | [] ->
          []
      | hd :: tl ->
          if hd = subscription then tl else hd :: check_and_remove tl
    in
    match t.socket_type with
    | SUB | XSUB -> (
      match t.socket_states with
      | Sub {subscriptions} ->
          t.socket_states <- Sub {subscriptions= check_and_remove subscriptions} ;
          send_message_to_all_active_connections t.connections
            (unsubscription_frame subscription)
      | Xsub {subscriptions} ->
          t.socket_states
          <- Xsub {subscriptions= check_and_remove subscriptions} ;
          send_message_to_all_active_connections t.connections
            (unsubscription_frame subscription)
      | _ ->
          raise Should_Not_Reach )
    | _ ->
        raise
          (Incorrect_use_of_API "This socket does not support unsubscription!")

  type queue_fold_type = UNINITIALISED | Result of Connection.t

  let rec recv t =
    match t.socket_type with
    | REP -> (
      match t.socket_states with
      | Rep _ -> (
          if Queue.is_empty t.connections then
            Lwt.pause () >>= fun () -> recv t
          else
            (* Go through the queue of connections and check buffer *)
            find_connection_with_incoming_buffer t.connections
            >>= function
            | None ->
                Lwt.pause () >>= fun () -> recv t
            | Some connection -> (
                (* Reconstruct message from the connection *)
                get_address_envelope connection
                >>= function
                | None ->
                    Connection.close connection ;
                    rotate t.connections true ;
                    Lwt.pause () >>= fun () -> recv t
                | Some address_envelope -> (
                    get_frame_list connection
                    >>= function
                    | None ->
                        Connection.close connection ;
                        rotate t.connections true ;
                        Lwt.pause () >>= fun () -> recv t
                    | Some frames ->
                        t.socket_states
                        <- Rep
                             { if_received= true
                             ; last_received_connection_tag=
                                 Connection.get_tag connection
                             ; address_envelope } ;
                        Lwt.return (Data (Frame.splice_message_frames frames))
                    ) ) )
      | _ ->
          raise Should_Not_Reach )
    | REQ -> (
      match t.socket_states with
      | Req {if_sent; last_sent_connection_tag= tag} -> (
          if not if_sent then
            raise
              (Incorrect_use_of_API "Need to send a request before receiving")
          else
            let rec find_and_send connections =
              let head = !(Queue.peek connections) in
              if tag = Connection.get_tag head then
                if Connection.get_stage head = TRAFFIC then (
                  get_frame_list head
                  >>= function
                  | None ->
                      Lwt.return None
                  | Some frames ->
                      t.socket_states
                      <- Req {if_sent= false; last_sent_connection_tag= ""} ;
                      Lwt.return (Some (Frame.splice_message_frames frames)) )
                else (
                  rotate t.connections true ;
                  t.socket_states
                  <- Req {if_sent= false; last_sent_connection_tag= ""} ;
                  raise Connection_closed )
              else
                raise
                  (Internal_Error "Receive target no longer at head of queue")
            in
            find_and_send t.connections
            >>= function
            | Some result ->
                rotate t.connections false ; Lwt.return (Data result)
            | None ->
                rotate t.connections true ;
                t.socket_states
                <- Req {if_sent= false; last_sent_connection_tag= ""} ;
                raise Connection_closed )
      | _ ->
          raise Should_Not_Reach )
    | DEALER -> (
      match t.socket_states with
      | Dealer {request_order_queue} -> (
          if Queue.is_empty request_order_queue then
            raise (Incorrect_use_of_API "You need to send requests first!")
          else
            let tag = Queue.peek request_order_queue in
            find_connection t.connections (fun connection ->
                Connection.get_tag connection = tag )
            |> function
            | None ->
                Lwt.pause () >>= fun () -> recv t
            | Some connection -> (
                (* Reconstruct message from the connection *)
                get_frame_list connection
                >>= function
                | None ->
                    Connection.close connection ;
                    ignore (Queue.pop request_order_queue) ;
                    raise Connection_closed
                | Some frames ->
                    (* Put the received connection at the end of the queue *)
                    ignore (Queue.pop request_order_queue) ;
                    Lwt.return (Data (Frame.splice_message_frames frames)) ) )
      | _ ->
          raise Should_Not_Reach )
    | ROUTER -> (
      match t.socket_states with
      | Router -> (
          if Queue.is_empty t.connections then
            Lwt.pause () >>= fun () -> recv t
          else
            find_connection_with_incoming_buffer t.connections
            >>= function
            | None ->
                Lwt.pause () >>= fun () -> recv t
            | Some connection -> (
                (* Reconstruct message from the connection *)
                get_frame_list connection
                >>= function
                | None ->
                    Connection.close connection ;
                    rotate t.connections true ;
                    Lwt.pause () >>= fun () -> recv t
                | Some frames ->
                    (* Put the received connection at the end of the queue *)
                    rotate t.connections false ;
                    Lwt.return
                      (Identity_and_data
                         ( Connection.get_identity connection
                         , Frame.splice_message_frames frames )) ) )
      | _ ->
          raise Should_Not_Reach )
    | PUB ->
        raise (Incorrect_use_of_API "Cannot receive from PUB")
    | SUB -> (
      match t.socket_states with
      | Sub {subscriptions= _} ->
          receive_and_rotate t.connections
      | _ ->
          raise Should_Not_Reach )
    | XPUB -> (
      match t.socket_states with
      | Xpub ->
          receive_and_rotate t.connections
      | _ ->
          raise Should_Not_Reach )
    | XSUB -> (
      match t.socket_states with
      | Xsub {subscriptions= _} ->
          receive_and_rotate t.connections
      | _ ->
          raise Should_Not_Reach )
    | PUSH ->
        raise (Incorrect_use_of_API "Cannot receive from PUSH")
    | PULL -> (
      match t.socket_states with
      | Pull ->
          receive_and_rotate t.connections
      | _ ->
          raise Should_Not_Reach )
    | PAIR -> (
      match t.socket_states with
      | Pair {connected} ->
          if Queue.is_empty t.connections then raise No_Available_Peers
          else
            let connection = !(Queue.peek t.connections) in
            if connected && Connection.get_stage connection = TRAFFIC then
              get_frame_list connection
              >>= function
              | None ->
                  Connection.close connection ;
                  t.socket_states <- Pair {connected= false} ;
                  rotate t.connections true ;
                  raise Connection_closed
              | Some frames ->
                  Lwt.return (Data (Frame.splice_message_frames frames))
            else raise No_Available_Peers
      | _ ->
          raise Should_Not_Reach )

  let send t msg =
    let frames =
      match msg with
      | Data msg ->
          List.map (fun x -> Message.to_frame x) (Message.list_of_string msg)
      | Identity_and_data (id, msg) ->
          List.map (fun x -> Message.to_frame x) (Message.list_of_string msg)
    in
    let rec try_send () =
      match t.socket_type with
      | REP -> (
        match msg with
        | Data msg -> (
            let state = t.socket_states in
            match state with
            | Rep
                { if_received
                ; last_received_connection_tag= tag
                ; address_envelope } ->
                if not if_received then
                  raise
                    (Incorrect_use_of_API
                       "Need to receive a request before sending a message")
                else
                  let rec find_and_send connections =
                    let head = !(Queue.peek connections) in
                    if tag = Connection.get_tag head then
                      if Connection.get_stage head = TRAFFIC then (
                        Connection.send head
                          (address_envelope @ (Frame.delimiter_frame :: frames))
                        >>= fun () ->
                        t.socket_states
                        <- Rep
                             { if_received= false
                             ; last_received_connection_tag= ""
                             ; address_envelope= [] } ;
                        rotate t.connections false ;
                        Lwt.return_unit )
                      else Lwt.return_unit
                    else
                      raise
                        (Internal_Error
                           "Send target no longer at head of queue")
                  in
                  find_and_send t.connections
            | _ ->
                raise Should_Not_Reach )
        | _ ->
            raise (Incorrect_use_of_API "REP sends [Data(string)]") )
      | REQ -> (
        match msg with
        | Data msg -> (
            let state = t.socket_states in
            match state with
            | Req {if_sent; last_sent_connection_tag= _} -> (
                if if_sent then
                  raise
                    (Incorrect_use_of_API
                       "Need to receive a reply before sending another message")
                else
                  find_available_connection t.connections
                  |> function
                  | None ->
                      raise No_Available_Peers
                  | Some connection ->
                      (* TODO check re-send is working *)
                      Queue.clear (Connection.get_read_buffer connection) ;
                      Connection.send connection
                        (Frame.delimiter_frame :: frames)
                      >>= fun () ->
                      t.socket_states
                      <- Req
                           { if_sent= true
                           ; last_sent_connection_tag=
                               Connection.get_tag connection } ;
                      Lwt.return_unit )
            | _ ->
                raise Should_Not_Reach )
        | _ ->
            raise (Incorrect_use_of_API "REP sends [Data(string)]") )
      | DEALER -> (
        (* TODO investigate DEALER dropping messages *)
        match msg with
        | Data msg -> (
            let state = t.socket_states in
            match state with
            | Dealer {request_order_queue} -> (
                if Queue.is_empty t.connections then raise No_Available_Peers
                else
                  find_available_connection t.connections
                  |> function
                  | None ->
                      raise No_Available_Peers
                  | Some connection -> (
                    try
                      Connection.send connection ~wait_until_sent:true
                        (Frame.delimiter_frame :: frames)
                      >>= fun () ->
                      Queue.push
                        (Connection.get_tag connection)
                        request_order_queue ;
                      Logs.debug (fun f -> f "Message sent") ;
                      rotate t.connections false ;
                      Lwt.return_unit
                    with Queue_overflow ->
                      Lwt.pause () >>= fun () -> try_send () ) )
            | _ ->
                raise Should_Not_Reach )
        | _ ->
            raise (Incorrect_use_of_API "DEALER sends [Data(string)]") )
      | ROUTER -> (
        match msg with
        | Identity_and_data (id, msg) -> (
          match t.socket_states with
          | Router -> (
              find_connection t.connections (fun connection ->
                  Connection.get_identity connection = id )
              |> function
              | None ->
                  Lwt.return_unit
              | Some connection -> (
                  let frame_list =
                    if Connection.get_incoming_socket_type connection == REQ
                    then Frame.delimiter_frame :: frames
                    else frames
                  in
                  try Connection.send connection frame_list
                  with (* Drop message when queue full *)
                  | Queue_overflow ->
                    Lwt.return_unit ) )
          | _ ->
              raise Should_Not_Reach )
        | _ ->
            raise
              (Incorrect_use_of_API
                 "Sending a message via ROUTER needs a specified receiver \
                  identity!") )
      | PUB -> (
        match msg with
        | Data msg -> (
          match t.socket_states with
          | Pub ->
              broadcast t.connections msg (fun connection ->
                  match_subscriptions msg
                    (Connection.get_subscriptions connection) ) ;
              Lwt.return_unit
          | _ ->
              raise Should_Not_Reach )
        | _ ->
            raise (Incorrect_use_of_API "PUB accepts a message only!") )
      | SUB ->
          raise (Incorrect_use_of_API "Cannot send via SUB")
      | XPUB -> (
        match msg with
        | Data msg -> (
          match t.socket_states with
          | Xpub ->
              broadcast t.connections msg (fun connection ->
                  match_subscriptions msg
                    (Connection.get_subscriptions connection) ) ;
              Lwt.return_unit
          | _ ->
              raise Should_Not_Reach )
        | _ ->
            raise (Incorrect_use_of_API "XPUB accepts a message only!") )
      | XSUB -> (
        match msg with
        | Data msg -> (
          match t.socket_states with
          | Xsub {subscriptions= _} ->
              broadcast t.connections msg (fun _ -> true) ;
              Lwt.return_unit
          | _ ->
              raise Should_Not_Reach )
        | _ ->
            raise (Incorrect_use_of_API "XSUB accepts a message only!") )
      | PUSH -> (
        match msg with
        | Data msg -> (
          match t.socket_states with
          | Push -> (
              if Queue.is_empty t.connections then raise No_Available_Peers
              else
                find_available_connection t.connections
                |> function
                | None ->
                    raise No_Available_Peers
                | Some connection -> (
                  try
                    Connection.send connection frames
                    >>= fun () -> rotate t.connections false ; Lwt.return_unit
                  with Queue_overflow -> try_send () ) )
          | _ ->
              raise Should_Not_Reach )
        | _ ->
            raise (Incorrect_use_of_API "PUSH accepts a message only!") )
      | PULL ->
          raise (Incorrect_use_of_API "Cannot send via PULL")
      | PAIR -> (
        match msg with
        | Data msg -> (
          match t.socket_states with
          | Pair {connected} ->
              if Queue.is_empty t.connections then raise No_Available_Peers
              else
                let connection = !(Queue.peek t.connections) in
                if
                  connected
                  && Connection.get_stage connection = TRAFFIC
                  && not (Connection.if_send_queue_full connection)
                then
                  try Connection.send connection frames
                  with Queue_overflow -> try_send ()
                else raise No_Available_Peers
          | _ ->
              raise Should_Not_Reach )
        | _ ->
            raise (Incorrect_use_of_API "PUSH accepts a message only!") )
    in
    try_send ()

  let rec send_blocking t msg =
    try send t msg
    with No_Available_Peers -> Lwt.pause () >>= fun () -> send_blocking t msg

  let add_connection t connection = Queue.push connection t.connections

  let initial_traffic_messages t =
    match t.socket_type with
    | SUB -> (
      match t.socket_states with
      | Sub {subscriptions} ->
          if subscriptions <> [] then
            List.map (fun x -> subscription_frame x) subscriptions
          else []
      | _ ->
          raise Should_Not_Reach )
    | XSUB -> (
      match t.socket_states with
      | Xsub {subscriptions} ->
          if subscriptions <> [] then
            List.map (fun x -> subscription_frame x) subscriptions
          else []
      | _ ->
          raise Should_Not_Reach )
    | _ ->
        []
end

and Security_mechanism : sig
  type t

  type action =
    | Write of bytes
    | Continue
    | Close
    | Received_property of string * string
    | Ok

  val get_name_string : t -> string
  (** Get the string description of the mechanism *)

  val get_as_server : t -> bool
  (** Whether the socket is a PLAIN server (always false if mechanism is NULL) *)

  val get_as_client : t -> bool
  (** Whether the socket is a PLAIN client (always false if mechanism is NULL) *)

  val if_send_command_after_greeting : t -> bool

  val init : security_data -> socket_metadata -> t
  (** Initialise a t from security mechanism data and socket metadata *)

  val client_first_message : t -> bytes
  (** If the socket is a PLAIN mechanism client, it needs to send the HELLO command first *)

  val first_command : t -> bytes

  val fsm : t -> Command.t -> t * action list
  (** FSM for handling the handshake *)
end = struct
  type state = START | START_SERVER | START_CLIENT | WELCOME | INITIATE | OK

  type action =
    | Write of bytes
    | Continue
    | Close
    | Received_property of string * string
    | Ok

  type t =
    { mechanism_type: mechanism_type
    ; socket_metadata: socket_metadata
    ; state: state
    ; data: security_data
    ; as_server: bool
    ; as_client: bool }

  (* Start of helper functions *)

  (** Makes an ERROR command *)
  let error error_reason =
    Frame.to_bytes
      (Command.to_frame
         (Command.make_command "ERROR"
            (Bytes.cat
               (Bytes.make 1 (Char.chr (String.length error_reason)))
               (Bytes.of_string error_reason))))

  (** Extracts metadata from a command *)
  let rec extract_metadata bytes =
    let len = Bytes.length bytes in
    if len = 0 then []
    else
      let name_length = Char.code (Bytes.get bytes 0) in
      let name = Bytes.sub_string bytes 1 name_length in
      let property_length =
        Utils.network_order_to_int (Bytes.sub bytes (name_length + 1) 4)
      in
      let property =
        Bytes.sub_string bytes (name_length + 1 + 4) property_length
      in
      (name, property)
      :: extract_metadata
           (Bytes.sub bytes
              (name_length + 1 + 4 + property_length)
              (len - (name_length + 1 + 4 + property_length)))

  (** Extracts username and password from HELLO *)
  let extract_username_password bytes =
    let username_length = Char.code (Bytes.get bytes 0) in
    let username = Bytes.sub bytes 1 username_length in
    let password_length = Char.code (Bytes.get bytes (1 + username_length)) in
    let password = Bytes.sub bytes (2 + username_length) password_length in
    (username, password)

  (** Makes a WELCOME command for a PLAIN server *)
  let welcome =
    Frame.to_bytes
      (Command.to_frame (Command.make_command "WELCOME" Bytes.empty))

  (** Makes a HELLO command for a PLAIN client *)
  let hello username password =
    Frame.to_bytes
      (Command.to_frame
         (Command.make_command "HELLO"
            (Bytes.concat Bytes.empty
               [ Bytes.make 1 (Char.chr (String.length username))
               ; Bytes.of_string username
               ; Bytes.make 1 (Char.chr (String.length password))
               ; Bytes.of_string password ])))

  (** Makes a new handshake for NULL mechanism *)
  let new_handshake_null metadata =
    let bytes_of_metadata (name, property) =
      let name_length = String.length name in
      let property_length = String.length property in
      Bytes.cat
        (Bytes.cat (Bytes.make 1 (Char.chr name_length)) (Bytes.of_string name))
        (Bytes.cat
           (Utils.int_to_network_order property_length 4)
           (Bytes.of_string property))
    in
    let rec convert_metadata = function
      | [] ->
          Bytes.empty
      | hd :: tl ->
          Bytes.cat (bytes_of_metadata hd) (convert_metadata tl)
    in
    Frame.to_bytes
      (Command.to_frame
         (Command.make_command "READY" (convert_metadata metadata)))

  (** Makes a metadata command (command = "INITIATE"/"READY") for PLAIN mechanism *)
  let metadata_command command metadata =
    let bytes_of_metadata (name, property) =
      let name_length = String.length name in
      let property_length = String.length property in
      Bytes.cat
        (Bytes.cat (Bytes.make 1 (Char.chr name_length)) (Bytes.of_string name))
        (Bytes.cat
           (Utils.int_to_network_order property_length 4)
           (Bytes.of_string property))
    in
    let rec convert_metadata = function
      | [] ->
          Bytes.empty
      | hd :: tl ->
          Bytes.cat (bytes_of_metadata hd) (convert_metadata tl)
    in
    Frame.to_bytes
      (Command.to_frame
         (Command.make_command command (convert_metadata metadata)))

  (* End of helper functions *)

  let get_name_string t =
    match t.mechanism_type with NULL -> "NULL" | PLAIN -> "PLAIN"

  let init security_data socket_metadata =
    match security_data with
    | Null ->
        { mechanism_type= NULL
        ; socket_metadata
        ; state= START
        ; data= security_data
        ; as_server= false
        ; as_client= false }
    | Plain_client _ ->
        { mechanism_type= PLAIN
        ; socket_metadata
        ; state= START_CLIENT
        ; data= security_data
        ; as_server= false
        ; as_client= true }
    | Plain_server _ ->
        { mechanism_type= PLAIN
        ; socket_metadata
        ; state= START_SERVER
        ; data= security_data
        ; as_server= true
        ; as_client= false }

  let client_first_message t =
    match t.mechanism_type with
    | NULL ->
        Bytes.empty
    | PLAIN ->
        if t.as_client then
          match t.data with
          | Plain_client (u, p) ->
              hello u p
          | _ ->
              raise (Internal_Error "Security mechanism mismatch")
        else Bytes.empty

  let fsm t command =
    let name = Command.get_name command in
    let data = Command.get_data command in
    match t.mechanism_type with
    | NULL -> (
      match t.state with
      | START -> (
        match name with
        | "READY" ->
            ( {t with state= OK}
            , List.map
                (fun (name, property) -> Received_property (name, property))
                (extract_metadata data)
              @ [Ok] )
        | "ERROR" ->
            ( {t with state= OK}
            , [Write (error "ERROR command received"); Close] )
        | _ ->
            ( {t with state= OK}
            , [Write (error "unknown command received"); Close] ) )
      | _ ->
          raise Should_Not_Reach )
    | PLAIN -> (
      match t.state with
      | START_SERVER ->
          if name = "HELLO" then
            let username, password = extract_username_password data in
            match t.data with
            | Plain_client _ ->
                raise (Internal_Error "Server data expected")
            | Plain_server hashtable -> (
              match Hashtbl.find_opt hashtable (Bytes.to_string username) with
              | Some valid_password ->
                  if valid_password = Bytes.to_string password then
                    ({t with state= WELCOME}, [Write welcome])
                  else
                    ( {t with state= OK}
                    , [Write (error "Handshake error"); Close] )
              | None ->
                  ({t with state= OK}, [Write (error "Handshake error"); Close])
              )
            | _ ->
                raise (Internal_Error "Security type mismatch")
          else ({t with state= OK}, [Write (error "Handshake error"); Close])
      | START_CLIENT ->
          if name = "WELCOME" then
            ( {t with state= INITIATE}
            , [Write (metadata_command "INITIATE" t.socket_metadata)] )
          else raise (Internal_Error "Wrong credentials")
      | WELCOME ->
          if name = "INITIATE" then
            ( {t with state= OK}
            , List.map
                (fun (name, property) -> Received_property (name, property))
                (extract_metadata data)
              @ [Write (metadata_command "READY" t.socket_metadata); Ok] )
          else ({t with state= OK}, [Write (error "Handshake error"); Close])
      | INITIATE ->
          if name = "READY" then
            ( {t with state= OK}
            , List.map
                (fun (name, property) -> Received_property (name, property))
                (extract_metadata data)
              @ [Ok] )
          else ({t with state= OK}, [Write (error "Handshake error"); Close])
      | _ ->
          raise Should_Not_Reach )

  let get_as_server t = t.as_server

  let get_as_client t = t.as_client

  let if_send_command_after_greeting t = t.as_client || t.mechanism_type = NULL

  let first_command t =
    if t.mechanism_type = NULL then new_handshake_null t.socket_metadata
    else if t.mechanism_type = PLAIN then client_first_message t
    else
      raise (Internal_Error "This socket should not send the command first.")
end

and Greeting : sig
  type t

  type event =
    | Recv_sig of bytes
    | Recv_Vmajor of bytes
    | Recv_Vminor of bytes
    | Recv_Mechanism of bytes
    | Recv_as_server of bytes
    | Recv_filler
    | Init of string

  type action =
    | Check_mechanism of string
    | Set_server of bool
    | Continue
    | Ok
    | Error of string

  val init : Security_mechanism.t -> t
  (** Initialise a t from a security mechanism to be used *)

  val fsm_single : t -> event -> t * action
  (** FSM call for handling a single event *)

  val fsm : t -> event list -> t * action list
  (** FSM call for handling a list of events. *)

  val new_greeting : Security_mechanism.t -> Bytes.t
end = struct
  type state =
    | START
    | SIGNATURE
    | VERSION_MAJOR
    | VERSION_MINOR
    | MECHANISM
    | AS_SERVER
    | SUCCESS
    | ERROR

  type t = {security: Security_mechanism.t; state: state}

  type event =
    | Recv_sig of bytes
    | Recv_Vmajor of bytes
    | Recv_Vminor of bytes
    | Recv_Mechanism of bytes
    | Recv_as_server of bytes
    | Recv_filler
    | Init of string

  type action =
    | Check_mechanism of string
    | Set_server of bool
    | Continue
    | Ok
    | Error of string

  type version = {major: bytes; minor: bytes}

  type greeting =
    { signature: bytes
    ; version: version
    ; mechanism: string
    ; as_server: bool
    ; filler: bytes }

  (* Start of helper functions *)

  (** Make the signature bytes *)
  let signature =
    let s = Bytes.make 10 (Char.chr 0) in
    Bytes.set s 0 (Char.chr 255) ;
    Bytes.set s 9 (Char.chr 127) ;
    s

  (** The default version of this implementation is 3.0 (RFC 23/ZMTP) *)
  let version =
    {major= Bytes.make 1 (Char.chr 3); minor= Bytes.make 1 (Char.chr 0)}

  (** Pad the mechanism string to 20 bytes *)
  let pad_mechanism m =
    let b = Bytes.of_string m in
    if Bytes.length b < 20 then
      Bytes.cat b (Bytes.make (20 - Bytes.length b) (Char.chr 0))
    else b

  (** Get the actual mechanism from null padded string *)
  let trim_mechanism m =
    let len = ref (Bytes.length m) in
    while Bytes.get m (!len - 1) = Char.chr 0 do
      len := !len - 1
    done ;
    Bytes.sub m 0 !len

  (** Makes the filler bytes *)
  let filler = Bytes.make 31 (Char.chr 0)

  (** Generates a new greeting *)
  let new_greeting security =
    Bytes.concat Bytes.empty
      [ signature
      ; version.major
      ; version.minor
      ; pad_mechanism (Security_mechanism.get_name_string security)
      ; ( if Security_mechanism.get_as_server security then
          Bytes.make 1 (Char.chr 1)
        else Bytes.make 1 (Char.chr 0) )
      ; filler ]

  (* End of helper functions *)

  let init security_t = {security= security_t; state= START}

  let fsm_single t event =
    match (t.state, event) with
    | START, Recv_sig b ->
        if Bytes.get b 0 = Char.chr 255 && Bytes.get b 9 = Char.chr 127 then
          ({t with state= SIGNATURE}, Continue)
        else ({t with state= ERROR}, Error "Protocol Signature not detected.")
    | SIGNATURE, Recv_Vmajor b ->
        if Bytes.get b 0 = Char.chr 3 then
          ({t with state= VERSION_MAJOR}, Continue)
        else ({t with state= ERROR}, Error "Version-major is not 3.")
    | VERSION_MAJOR, Recv_Vminor b ->
        if Bytes.get b 0 = Char.chr 0 then
          ({t with state= VERSION_MINOR}, Continue)
        else ({t with state= ERROR}, Error "Version-minor is not 0.")
    | VERSION_MINOR, Recv_Mechanism b ->
        ( {t with state= MECHANISM}
        , Check_mechanism (Bytes.to_string (trim_mechanism b)) )
    | MECHANISM, Recv_as_server b ->
        if Bytes.get b 0 = Char.chr 0 then
          ({t with state= AS_SERVER}, Set_server false)
        else ({t with state= AS_SERVER}, Set_server true)
    | AS_SERVER, Recv_filler ->
        ({t with state= SUCCESS}, Ok)
    | _ ->
        ({t with state= ERROR}, Error "Unexpected event.")

  let fsm t event_list =
    let rec fsm_accumulator t event_list action_list =
      match event_list with
      | [] -> (
        match t.state with
        | ERROR ->
            ({t with state= ERROR}, [List.hd action_list])
        | _ ->
            (t, List.rev action_list) )
      | hd :: tl -> (
        match t.state with
        | ERROR ->
            ({t with state= ERROR}, [List.hd action_list])
        | _ ->
            let new_state, action = fsm_single t hd in
            fsm_accumulator new_state tl (action :: action_list) )
    in
    fsm_accumulator t event_list []
end

and Connection : sig
  type t

  type action = Write of bytes | Close of string | Block

  val init : Socket.t ref -> Security_mechanism.t -> string -> t
  (** Create a new connection for socket with specified security mechanism *)

  val get_tag : t -> string
  (** Get the unique tag used to identify the connection *)

  val get_read_buffer : t -> Frame.t option Queue.t
  (** Get the read buffer of the connection *)

  val get_write_buffer : t -> Bytes.t option Queue.t
  (** Get the output buffer of the connection *)

  val get_stage : t -> connection_stage
  (** Get the stage of the connection. It is considered usable if in TRAFFIC *)

  val get_socket : t -> Socket.t ref

  val get_identity : t -> string

  val get_subscriptions : t -> string list

  val get_incoming_socket_type : t -> socket_type

  val greeting_message : t -> Bytes.t

  val fsm : t -> connection_fsm_data -> action list
  (** FSM for handing raw bytes transmission *)

  val send : t -> ?wait_until_sent:bool -> Frame.t list -> unit Lwt.t
  (** Send the list of frames to underlying connection *)

  val close : t -> unit
  (** Force close connection *)

  val if_send_queue_full : t -> bool
  (** Returns whether the send queue is full (always false if unbounded size *)
end = struct
  type action = Write of bytes | Close of string | Block

  type subscription_message = Subscribe | Unsubscribe | Ignore

  type t =
    { tag: string
    ; socket: Socket.t ref
    ; mutable greeting_state: Greeting.t
    ; mutable handshake_state: Security_mechanism.t
    ; mutable stage: connection_stage
    ; mutable expected_bytes_length: int
    ; mutable incoming_as_server: bool
    ; mutable incoming_socket_type: socket_type
    ; mutable incoming_identity: string
    ; read_buffer: Frame.t option Queue.t
    ; send_buffer: Bytes.t option Queue.t
    ; mutable subscriptions: string list
    ; mutable previous_fragment: Bytes.t }

  let init socket security_mechanism tag =
    { tag
    ; socket
    ; greeting_state= Greeting.init security_mechanism
    ; handshake_state= security_mechanism
    ; stage= GREETING
    ; expected_bytes_length= 64
    ; (* A value of 0 means expecting a frame of any length; starting with expectint the whole greeting *)
      incoming_socket_type= REP
    ; incoming_as_server= false
    ; incoming_identity= tag
    ; read_buffer= Queue.create ()
    ; send_buffer= Queue.create ()
    ; subscriptions= []
    ; previous_fragment= Bytes.empty }

  let get_tag t = t.tag

  let get_stage t = t.stage

  let get_read_buffer t = t.read_buffer

  let get_write_buffer t = t.send_buffer

  let get_socket t = t.socket

  let get_identity t = t.incoming_identity

  let get_subscriptions t = t.subscriptions

  let get_incoming_socket_type t = t.incoming_socket_type

  let greeting_message t = Greeting.new_greeting t.handshake_state

  let rec fsm t data =
    match data with
    | End_of_connection ->
        Queue.push None t.read_buffer ;
        []
    | Input_data bytes -> (
      match t.stage with
      | GREETING -> (
          let if_pair =
            match Socket.get_socket_type !(t.socket) with
            | PAIR ->
                true
            | _ ->
                false
          in
          let if_pair_already_connected =
            match Socket.get_socket_type !(t.socket) with
            | PAIR ->
                Socket.get_pair_connected !(t.socket)
            | _ ->
                false
          in
          (* Logs.debug (fun f -> f "Module Connection: Greeting -> FSM\n") ;*)
          if if_pair then Socket.set_pair_connected !(t.socket) true ;
          let len = Bytes.length bytes in
          let rec convert greeting_action_list =
            match greeting_action_list with
            | [] ->
                []
            | hd :: tl -> (
              match hd with
              | Greeting.Set_server b ->
                  t.incoming_as_server <- b ;
                  if
                    t.incoming_as_server
                    && Security_mechanism.get_as_server t.handshake_state
                  then [Close "Both ends cannot be servers"]
                  else if
                    Security_mechanism.get_as_client t.handshake_state
                    && not t.incoming_as_server
                  then [Close "Other end is not a server"]
                  else convert tl
              (* Assume security mechanism is pre-set*)
              | Greeting.Check_mechanism s ->
                  if s <> Security_mechanism.get_name_string t.handshake_state
                  then [Close "Security Policy mismatch"]
                  else convert tl
              | Greeting.Continue ->
                  convert tl
              | Greeting.Ok ->
                  (* Logs.debug (fun f -> f "Module Connection: Greeting OK\n") ; *)
                  t.stage <- HANDSHAKE ;
                  if
                    Security_mechanism.if_send_command_after_greeting
                      t.handshake_state
                  then
                    Write (Security_mechanism.first_command t.handshake_state)
                    :: convert tl
                  else convert tl
              | Greeting.Error s ->
                  [Close ("Greeting FSM error: " ^ s)] )
          in
          match len with
          (* Hard code the length here. The greeting is either complete or split into 11 + 53 or 10 + 54 *)
          (* Full greeting *)
          | 64 ->
              let state, action_list =
                Greeting.fsm t.greeting_state
                  [ Greeting.Recv_sig (Bytes.sub bytes 0 10)
                  ; Greeting.Recv_Vmajor (Bytes.sub bytes 10 1)
                  ; Greeting.Recv_Vminor (Bytes.sub bytes 11 1)
                  ; Greeting.Recv_Mechanism (Bytes.sub bytes 12 20)
                  ; Greeting.Recv_as_server (Bytes.sub bytes 32 1)
                  ; Greeting.Recv_filler ]
              in
              let connection_action = convert action_list in
              if if_pair_already_connected then
                [Close "This PAIR is already connected"]
              else (
                t.greeting_state <- state ;
                t.expected_bytes_length <- 0 ;
                connection_action )
          (* Signature + version major *)
          | 11 ->
              let state, action_list =
                Greeting.fsm t.greeting_state
                  [ Greeting.Recv_sig (Bytes.sub bytes 0 10)
                  ; Greeting.Recv_Vmajor (Bytes.sub bytes 10 1) ]
              in
              if if_pair_already_connected then
                [Close "This PAIR is already connected"]
              else (
                t.greeting_state <- state ;
                t.expected_bytes_length <- 53 ;
                convert action_list )
          (* Signature *)
          | 10 ->
              let state, action =
                Greeting.fsm_single t.greeting_state (Greeting.Recv_sig bytes)
              in
              if if_pair_already_connected then
                [Close "This PAIR is already connected"]
              else (
                t.greeting_state <- state ;
                t.expected_bytes_length <- 54 ;
                convert [action] )
          (* version minor + rest *)
          | 53 ->
              let state, action_list =
                Greeting.fsm t.greeting_state
                  [ Greeting.Recv_Vminor (Bytes.sub bytes 0 1)
                  ; Greeting.Recv_Mechanism (Bytes.sub bytes 1 20)
                  ; Greeting.Recv_as_server (Bytes.sub bytes 21 1)
                  ; Greeting.Recv_filler ]
              in
              let connection_action = convert action_list in
              t.greeting_state <- state ;
              t.expected_bytes_length <- 0 ;
              connection_action
          (* version major + rest *)
          | 54 ->
              let state, action_list =
                Greeting.fsm t.greeting_state
                  [ Greeting.Recv_Vmajor (Bytes.sub bytes 0 1)
                  ; Greeting.Recv_Vminor (Bytes.sub bytes 1 1)
                  ; Greeting.Recv_Mechanism (Bytes.sub bytes 2 20)
                  ; Greeting.Recv_as_server (Bytes.sub bytes 22 1)
                  ; Greeting.Recv_filler ]
              in
              let connection_action = convert action_list in
              t.greeting_state <- state ;
              t.expected_bytes_length <- 0 ;
              connection_action
          | n ->
              if n < t.expected_bytes_length then [Close "Message too short"]
              else
                let expected_length = t.expected_bytes_length in
                (* Handle greeting part *)
                let action_list_1 =
                  fsm t (Input_data (Bytes.sub bytes 0 expected_length))
                in
                (* Handle handshake part *)
                let action_list_2 =
                  fsm t
                    (Input_data
                       (Bytes.sub bytes expected_length (n - expected_length)))
                in
                action_list_1 @ action_list_2 )
      | HANDSHAKE -> (
          (* Logs.debug (fun f -> f "Module Connection: Handshake -> FSM\n") ; *)
          let frames, fragment =
            Frame.list_of_bytes (Bytes.cat t.previous_fragment bytes)
          in
          match frames with
          | [] ->
              t.previous_fragment <- fragment ;
              []
          | frame :: _ ->
              (* assume only one command is received at a time *)
              t.previous_fragment <- fragment ;
              let command = Command.of_frame frame in
              let new_state, actions =
                Security_mechanism.fsm t.handshake_state command
              in
              let rec convert handshake_action_list =
                match handshake_action_list with
                | [] ->
                    []
                | hd :: tl -> (
                  match hd with
                  | Security_mechanism.Write b ->
                      Write b :: convert tl
                  | Security_mechanism.Continue ->
                      convert tl
                  | Security_mechanism.Ok ->
                      (* Logs.debug (fun f -> f "Module Connection: Handshake OK\n") ; *)
                      t.stage <- TRAFFIC ;
                      let frames =
                        Socket.initial_traffic_messages !(t.socket)
                      in
                      List.map (fun x -> Write (Frame.to_bytes x)) frames
                      @ convert tl
                  | Security_mechanism.Close ->
                      [Close "Handshake FSM error"]
                  | Security_mechanism.Received_property (name, value) -> (
                    match name with
                    | "Socket-Type" ->
                        if
                          Socket.if_valid_socket_pair
                            (Socket.get_socket_type !(t.socket))
                            (Socket.socket_type_from_string value)
                        then (
                          t.incoming_socket_type
                          <- Socket.socket_type_from_string value ;
                          convert tl )
                        else [Close "Socket type mismatch"]
                    | "Identity" ->
                        t.incoming_identity <- value ;
                        convert tl
                    | _ ->
                        (* Logs.debug (fun f ->
                          f "Module Connection: Ignore unknown property %s\n"
                            name ) ;*)
                        convert tl ) )
              in
              let actions = convert actions in
              t.handshake_state <- new_state ;
              actions )
      | TRAFFIC -> (
          (* Logs.debug (fun f -> f "Module Connection: TRAFFIC -> FSM\n") ; *)
          let frames, fragment =
            Frame.list_of_bytes (Bytes.cat t.previous_fragment bytes)
          in
          t.previous_fragment <- fragment ;
          let manage_subscription () =
            let match_subscription_signature frame =
              if
                (not (Frame.get_if_more frame))
                && (not (Frame.get_if_command frame))
                && not (Frame.get_if_long frame)
              then
                let first_char =
                  (Bytes.to_string (Frame.get_body frame)).[0]
                in
                if first_char = Char.chr 1 then Subscribe
                else if first_char = Char.chr 0 then Unsubscribe
                else Ignore
              else Ignore
            in
            List.iter
              (fun x ->
                match match_subscription_signature x with
                | Unsubscribe ->
                    let body = Bytes.to_string (Frame.get_body x) in
                    let sub = String.sub body 1 (String.length body - 1) in
                    let rec check_and_remove subscriptions =
                      match subscriptions with
                      | [] ->
                          []
                      | hd :: tl ->
                          if hd = sub then tl else hd :: check_and_remove tl
                    in
                    t.subscriptions <- check_and_remove t.subscriptions
                | Subscribe ->
                    let body = Bytes.to_string (Frame.get_body x) in
                    t.subscriptions
                    <- String.sub body 1 (String.length body - 1)
                       :: t.subscriptions
                | Ignore ->
                    () )
              frames
          in
          let enqueue () =
            (* Put the received frames into the buffer *)
            (* TODO limit incoming buffer *)
            let socket_type = Socket.get_socket_type !(t.socket) in
            if Socket.if_queue_size_limited socket_type then
              if
                Queue.length t.read_buffer
                < Socket.get_incoming_queue_size !(t.socket)
              then (
                List.iter (fun x -> Queue.push (Some x) t.read_buffer) frames ;
                [] )
              else [Block]
            else (
              List.iter (fun x -> Queue.push (Some x) t.read_buffer) frames ;
              [] )
          in
          match Socket.get_socket_type !(t.socket) with
          | PUB ->
              manage_subscription () ; []
          | XPUB -> (
              (* TODO check XPUB's behaviour *)
              enqueue ()
              |> function
              | [] ->
                  manage_subscription () ; []
              | [Block] ->
                  [Block]
              | _ ->
                  raise (Internal_Error "Should not reach") )
          | _ ->
              enqueue () )
      | CLOSED ->
          [Close "Connection FSM error"] )

  let close t =
    let if_pair =
      match Socket.get_socket_type !(t.socket) with PAIR -> true | _ -> false
    in
    if if_pair then Socket.set_pair_connected !(t.socket) false ;
    t.stage <- CLOSED ;
    Queue.push None t.send_buffer

  let send t ?(wait_until_sent = false) msg_list =
    if not (Socket.if_queue_size_limited (Socket.get_socket_type !(t.socket)))
    then (
      (* Unbounded sending queue *)
      List.iter
        (fun x -> Queue.push (Some (Frame.to_bytes x)) t.send_buffer)
        msg_list ;
      if wait_until_sent then
        let rec check () =
          if Queue.is_empty t.send_buffer then Lwt.return_unit
          else Lwt.pause () >>= fun () -> check ()
        in
        check ()
      else Lwt.return_unit )
    else if
      (* Sending queue of limited size *)
      Queue.length t.send_buffer < Socket.get_outgoing_queue_size !(t.socket)
    then (
      let bytes = List.map (fun x -> Frame.to_bytes x) msg_list in
      List.iter (fun x -> Queue.push (Some x) t.send_buffer) bytes ;
      if wait_until_sent then
        let rec check () =
          if Queue.is_empty t.send_buffer then Lwt.return_unit
          else Lwt.pause () >>= fun () -> check ()
        in
        check ()
      else Lwt.return_unit )
    else (* Overflowing sending queue *)
      raise Queue_overflow

  let if_send_queue_full t =
    Queue.length t.send_buffer >= Socket.get_outgoing_queue_size !(t.socket)
end

module Connection_tcp (S : Mirage_stack_lwt.V4) = struct
  (* Start of helper functions *)

  (** Creates a tag for a TCP connection *)
  let tag_of_tcp_connection ipaddr port =
    String.concat "." ["TCP"; ipaddr; string_of_int port]

  (** Read input from flow, send the input to FSM and execute FSM actions *)
  let rec process_input flow connection =
    S.TCPV4.read flow
    >>= function
    | Ok `Eof ->
        (* Logs.debug (fun f -> f "Module Connection_tcp: Closing connection EOF") ; *)
        ignore (Connection.fsm connection End_of_connection) ;
        Connection.close connection ;
        Lwt.return_unit
    | Error e ->
        (* Logs.warn (fun f ->
            f
              "Module Connection_tcp: Error reading data from established \
               connection: %a"
              S.TCPV4.pp_error e ) ; *)
        ignore (Connection.fsm connection End_of_connection) ;
        Connection.close connection ;
        Lwt.return_unit
    | Ok (`Data b) ->
        (* Logs.debug (fun f ->
            f "Module Connection_tcp: Read: %d bytes:\n%s" (Cstruct.len b)
              (Utils.buffer_to_string (Cstruct.to_bytes b)) ) ; *)
        let bytes = Cstruct.to_bytes b in
        let rec act () =
          let actions = Connection.fsm connection (Input_data bytes) in
          let rec deal_with_action_list actions =
            match actions with
            | [] ->
                Lwt.pause () >>= fun () -> process_input flow connection
            | hd :: tl -> (
              match hd with
              | Connection.Block ->
                  Lwt.pause () >>= fun () -> act ()
              | Connection.Write b -> (
                  (* Logs.debug (fun f ->
                    f
                      "Module Connection_tcp: Connection FSM Write %d bytes\n\
                       %s\n"
                      (Bytes.length b) (Utils.buffer_to_string b) ) ; *)
                  S.TCPV4.write flow (Cstruct.of_bytes b)
                  >>= function
                  | Error _ ->
                      (* Logs.warn (fun f ->
                        f
                          "Module Connection_tcp: Error writing data to \
                           established connection." ) ; *)
                      Lwt.return_unit
                  | Ok () ->
                      deal_with_action_list tl )
              | Connection.Close s ->
                  (* Logs.debug (fun f ->
                      f
                        "Module Connection_tcp: Connection FSM Close due to: %s\n"
                        s ) ; *)
                  Lwt.return_unit )
          in
          deal_with_action_list actions
        in
        act ()

  (** Check the 'mailbox' and send outgoing data / close connection *)
  let rec process_output flow connection =
    try
      Queue.pop (Connection.get_write_buffer connection)
      |> function
      | None ->
          (* Stream closed *)
          (* Logs.debug (fun f ->
              f "Module Connection_tcp: Connection was instructed to close" ) ; *)
          S.TCPV4.close flow
      | Some data -> (
          (* Logs.debug (fun f ->
              f
                "Module Connection_tcp: Connection mailbox Write %d bytes\n\
                 %s\n"
                (Bytes.length data)
                (Utils.buffer_to_string data) ) ; *)
          S.TCPV4.write flow (Cstruct.of_bytes data)
          >>= function
          | Error _ ->
              (* Logs.warn (fun f ->
                  f
                    "Module Connection_tcp: Error writing data to established \
                     connection." ) ;*)
              Connection.close connection ;
              Lwt.return_unit
          | Ok () ->
              process_output flow connection )
    with Queue.Empty ->
      Lwt.pause () >>= fun () -> process_output flow connection

  (* End of helper functions *)

  let start_connection flow connection =
    S.TCPV4.write flow
      (Cstruct.of_bytes (Connection.greeting_message connection))
    >>= function
    | Error _ ->
        (* Logs.warn (fun f ->
            f
              "Module Connection_tcp: Error writing data to established \
               connection." ) ; *)
        Lwt.return_unit
    | Ok () ->
        process_input flow connection

  let listen s port socket =
    S.listen_tcpv4 s ~port (fun flow ->
        let dst, dst_port = S.TCPV4.dst flow in
        (*Logs.debug (fun f ->
            f "Module Connection_tcp: New tcp connection from IP %s on port %d"
              (Ipaddr.V4.to_string dst) dst_port ) ; *)
        let connection =
          Connection.init socket
            (Security_mechanism.init
               (Socket.get_security_data !socket)
               (Socket.get_metadata !socket))
            (tag_of_tcp_connection (Ipaddr.V4.to_string dst) dst_port)
        in
        if
          Socket.if_has_outgoing_queue
            (Socket.get_socket_type !(Connection.get_socket connection))
        then (
          Socket.add_connection !socket (ref connection) ;
          Lwt.join
            [start_connection flow connection; process_output flow connection] )
        else (
          Socket.add_connection !socket (ref connection) ;
          start_connection flow connection ) ) ;
    S.listen s

  let rec connect s addr port connection =
    let ipaddr = Ipaddr.V4.of_string_exn addr in
    S.TCPV4.create_connection (S.tcpv4 s) (ipaddr, port)
    >>= function
    | Ok flow ->
        let socket_type =
          Socket.get_socket_type !(Connection.get_socket connection)
        in
        if Socket.if_has_outgoing_queue socket_type then
          Lwt.async (fun () ->
              Lwt.join
                [ start_connection flow connection
                ; process_output flow connection ] )
        else Lwt.async (fun () -> start_connection flow connection) ;
        let rec wait_until_traffic () =
          if Connection.get_stage connection <> TRAFFIC then
            Lwt.pause () >>= fun () -> wait_until_traffic ()
          else Lwt.return_unit
        in
        wait_until_traffic ()
    | Error e ->
        (* Logs.warn (fun f ->
            f
              "Module Connection_tcp: Error establishing connection: %a, \
               retrying"
              S.TCPV4.pp_error e ) ; *)
        connect s addr port connection
end

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
  (** Set the maximum capacity of the incoming queue *)

  val set_outgoing_queue_size : t -> int -> unit
  (** Set the maximum capacity of the outgoing queue *)

  val subscribe : t -> string -> unit

  val unsubscribe : t -> string -> unit

  val recv : t -> message Lwt.t
  (** Receive a msg from the underlying connections, according to the  semantics of the socket type *)

  val send : t -> message -> unit Lwt.t
  (** Send a msg to the underlying connections, according to the semantics of the socket type *)

  val send_blocking : t -> message -> unit Lwt.t
  (** Send a msg to the underlying connections. It blocks until a peer is available *)

  val bind : t -> int -> S.t -> unit
  (** Bind a local TCP port to the socket so the socket will accept incoming connections *)

  val connect : t -> string -> int -> S.t -> unit Lwt.t
  (** Bind a connection to a remote TCP port to the socket *)
end = struct
  type transport_info = Tcp of string * int

  type t = {socket: Socket.t}

  let create_socket context ?(mechanism = NULL) socket_type =
    {socket= Socket.create_socket context ~mechanism socket_type}

  let set_plain_credentials t username password =
    Socket.set_plain_credentials t.socket username password

  let set_plain_user_list t list = Socket.set_plain_user_list t.socket list

  let set_identity t identity = Socket.set_identity t.socket identity

  let set_incoming_queue_size t size =
    Socket.set_incoming_queue_size t.socket size

  let set_outgoing_queue_size t size =
    Socket.set_outgoing_queue_size t.socket size

  let subscribe t subscription = Socket.subscribe t.socket subscription

  let unsubscribe t subscription = Socket.unsubscribe t.socket subscription

  let recv t = Socket.recv t.socket

  let send t msg = Socket.send t.socket msg

  let send_blocking t msg = Socket.send_blocking t.socket msg

  let bind t port s =
    let module C_tcp = Connection_tcp (S) in
    Lwt.async (fun () -> C_tcp.listen s port (ref t.socket))

  let connect t ipaddr port s =
    let module C_tcp = Connection_tcp (S) in
    let connection =
      Connection.init (ref t.socket)
        (Security_mechanism.init
           (Socket.get_security_data t.socket)
           (Socket.get_metadata t.socket))
        (C_tcp.tag_of_tcp_connection ipaddr port)
    in
    Socket.add_connection t.socket (ref connection) ;
    C_tcp.connect s ipaddr port connection
end
