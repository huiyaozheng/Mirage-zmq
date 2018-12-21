open Lwt.Infix

exception Not_Implemented
exception Should_Not_Reach
exception Socket_Name_Not_Recognised
exception Not_Able_To_Set_Credentials
exception Internal_Error of string
exception Incorrect_use_of_API of string

type socket_type = REQ | REP | DEALER | ROUTER | PUB | XPUB | SUB | XSUB | PUSH | PULL | PAIR
(* CURVE not implemented *)
type mechanism_type = NULL | PLAIN
type socket_metadata = (string * string) list
type security_data = Null | Plain_client of string * string | Plain_server of (string * string) list
type connection_stage = GREETING | HANDSHAKE | TRAFFIC | CLOSED
type connection_buffer_object = Data of Bytes.t | Command_close

(* Start of helper functions *)
(** Returns the string of the socket type *)
let string_of_socket_type = function
    | REQ -> "REQ"
    | REP -> "REP"
    | DEALER -> "DEALER"
    | ROUTER -> "ROUTER"
    | PUB -> "PUB"
    | XPUB -> "XPUB"
    | SUB -> "SUB"
    | XSUB -> "XSUB"
    | PUSH -> "PUSH"
    | PULL -> "PULL"
    | PAIR -> "PAIR"

(** Returns the socket type from string *)
let socket_type_from_string = function
    | "REQ" -> REQ
    | "REP" -> REP
    | "DEALER" -> DEALER
    | "ROUTER" -> ROUTER
    | "PUB" -> PUB
    | "XPUB" -> XPUB
    | "SUB" -> SUB
    | "XSUB" -> XSUB
    | "PUSH" -> PUSH
    | "PULL" -> PULL
    | "PAIR" -> PAIR
    | _ -> raise Socket_Name_Not_Recognised

(** Checks if the pair is valid as specified by 23/ZMTP *)
let if_valid_socket_pair a b =
    match (a,b) with 
        | (REQ, REP) | (REQ, ROUTER)
        | (REP, REQ) | (REP, DEALER)
        | (DEALER, REP) | (DEALER, DEALER) | (DEALER, ROUTER)
        | (ROUTER, REQ) | (ROUTER, DEALER) | (ROUTER, ROUTER)
        | (PUB, SUB) | (PUB, XSUB) 
        | (XPUB, SUB) | (XPUB, XSUB)
        | (SUB, PUB) | (SUB, XPUB)
        | (XSUB, PUB) | (XSUB, XPUB)
        | (PUSH, PULL) | (PULL, PUSH)
        | (PAIR, PAIR) -> true
        | _ -> false 

(** Convert a series of big-endian bytes to int *)
let rec network_order_to_int bytes = 
    let length = Bytes.length bytes in
        if length = 1 then Char.code (Bytes.get bytes 0)
        else (Char.code (Bytes.get bytes (length - 1))) + (network_order_to_int (Bytes.sub bytes 0 (length - 1))) * 256

(** Convert a int to big-endian bytes of length n *)
let int_to_network_order n length =
    Bytes.init length (fun i -> (Char.chr (n lsr (8 * (length - i - 1)) land 255)))

(** Converts a byte buffer to printable string *)
let buffer_to_string data = 
let content = ref [] in
    Bytes.iter (fun b -> content := Char.code b :: !content) data;
    String.concat " "  (List.map (fun x -> if (x >= 65 && x <= 90) || (x >= 97 && x <= 122) then String.make 1 (Char.chr x) else string_of_int x)  (List.rev !content))

(** Creates a tag for a TCP connection *)
let tag_of_tcp_connection ipaddr port =
    String.concat "." ["TCP"; ipaddr; string_of_int port]
(* End of helper functions *)
module Frame : sig
    type t

    (** make_frame body ifMore ifCommand *)
    val make_frame : bytes -> if_more : bool -> if_command : bool -> t
    
    (** Convert a frame to raw bytes *)
    val to_bytes : t -> bytes
    
    (** Construct a frame from raw bytes of a complete frame *)
    val of_bytes : bytes -> t

    (** Construct a list of frames from raw bytes; used when potentially many frames in received buffer *)
    val list_of_bytes : bytes -> t list
    
    (** Get if_more field from a frame *)
    val get_if_more : t -> bool
    
    (** Get if_command field from a frame *)
    val get_if_command : t -> bool
    
    (** Get body from a frame *)
    val get_body : t -> bytes

    (** A helper function checking whether a frame is empty (delimiter) *)
    val is_delimiter_frame : t -> bool

    (** Make a delimiter frame*)
    val delimiter_frame : t

    (** A helper function that takes a list of message frames and returns the reconstructed message *)
    val splice_message_frames : t list -> string
end = struct
    type t = {
        flag : char; 
(* Known issue: size is limited by max_int; may not be able to reach 2^63-1 *)
        size : int; 
        body : bytes
    }

    let size_to_bytes size if_long = 
        if not if_long then Bytes.make 1 (Char.chr size)
        else int_to_network_order size 8

    let make_frame body ~if_more ~if_command = 
        let f = ref 0 in
        let len = Bytes.length body in
            if if_more then f := !f + 1;
            if if_command then f := !f + 4;
            if len > 255 then f := !f + 2;
            {flag = (Char.chr (!f)); size = len; body}

    let to_bytes t =
        Bytes.concat Bytes.empty [Bytes.make 1 t.flag; size_to_bytes t.size (((Char.code t.flag) land 2) = 2); t.body]

    let of_bytes bytes = 
    let flag = Char.code (Bytes.get bytes 0) in
    let total_length = Bytes.length bytes in
    let if_long = (flag land 2) = 2 in
        if if_long then
            (* long-size *)
(* TODO test long *)
            let content_length = network_order_to_int (Bytes.sub bytes 1 8) in
                if total_length < content_length + 9 then raise (Internal_Error "Not a complete frame buffer")
                else if total_length > content_length + 9 then raise (Internal_Error "More than one frame in the buffer")
                else {flag = Char.chr flag; size = content_length; body = Bytes.sub bytes 9 content_length}
        else
            (* short-size *)
            let content_length = Char.code (Bytes.get bytes 1) in
                (* check length *)
                if total_length < content_length + 2 then raise (Internal_Error "Not a complete frame buffer")
                else if total_length > content_length + 2 then raise (Internal_Error "More than one frame in the buffer")
                else {flag = Char.chr flag; size = content_length; body = Bytes.sub bytes 2 content_length}

    let list_of_bytes bytes = 
    let rec list_of_bytes_accumu bytes list = (
        let total_length = Bytes.length bytes in
        let content_length = Char.code (Bytes.get bytes 1) in
            if total_length < content_length + 2 then raise (Internal_Error "Not a complete frame buffer")
            else if total_length > content_length + 2 then list_of_bytes_accumu (Bytes.sub bytes (content_length + 2) (total_length - content_length - 2)) ((of_bytes (Bytes.sub bytes 0 (content_length + 2)))::list)
            else (of_bytes (Bytes.sub bytes 0 (content_length + 2)))::list
        )
    in
        List.rev (list_of_bytes_accumu bytes [])

    
    let get_if_more t = (Char.code(t.flag) land 1) = 1

    let get_if_command t = (Char.code(t.flag) land 4) = 4

    let get_body t = t.body

    let is_delimiter_frame t = (get_if_more t) && (t.size = 0)

    let delimiter_frame = {flag = Char.chr 1; size = 0; body = Bytes.empty}

    let splice_message_frames list = 
    let rec splice_message_frames_accumu list s = match list with 
        | [] -> s
        | hd::tl -> match tl with
                        | [] -> if get_if_more hd then raise (Internal_Error "Missing frames")
                                else splice_message_frames_accumu tl (s ^ (Bytes.to_string (get_body hd)))
                        | _ -> if not (get_if_more hd) then raise (Internal_Error "Too many  frames")
                               else splice_message_frames_accumu tl (s ^ (Bytes.to_string (get_body hd)))
    in splice_message_frames_accumu list ""
end

module Command : sig
    type t

    (** Convert a command to a frame *)
    val to_frame : t -> Frame.t
    
    (** Get name of the command (without length byte) *)
    val get_name : t -> string
    
    (** Get data of the command *)
    val get_data : t -> bytes
    
    (** Construct a command from the enclosing frame *)
    val of_frame : Frame.t -> t
    
    (** Construct a command from given name and data *)
    val make_command : string -> bytes -> t
end = struct
    type t = {name : string; data : bytes}
    let to_frame t = Frame.make_frame (Bytes.concat Bytes.empty [(Bytes.make 1 (Char.chr (String.length t.name))); Bytes.of_string t.name; t.data]) ~if_more:false ~if_command:true 
    let get_name t = t.name
    let get_data t = t.data
    
    let of_frame frame = 
    let data = Frame.get_body frame in
    let name_length = Char.code (Bytes.get data 0) in
        {name = Bytes.sub_string data 1 name_length; data = Bytes.sub data (name_length + 1) ((Bytes.length data) - 1 - name_length)}

    let make_command command_name data = {name = command_name; data = data}
end

module Message : sig
    type t

    (** Make a message from the string *)
    val of_string : string -> ?if_long : bool -> ?if_more : bool -> t

    (** Make a list of messages from the string to send *)
    val list_of_string : string -> t list

    (** Convert a message to a frame *)
    val to_frame : t -> Frame.t

end = struct
    type t = {size : int; if_long : bool; if_more : bool; body : bytes}

    let of_string msg ?(if_long = false) ?(if_more = false) =
    let length = String.length msg in
        if length > 255 && (not if_long) then raise (Internal_Error "Must be long message")
        else {size = length; if_long = if_long; if_more = if_more; body = Bytes.of_string msg}

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
                    make (String.sub msg 255 (length - 255)) ((of_string (String.sub msg 0 255) ~if_long:false ~if_more:true)::list)
                else
                    (of_string msg ~if_long:false ~if_more:false)::list
            in 
                List.rev (make msg [])

    let to_frame t = Frame.make_frame t.body ~if_more:t.if_more ~if_command:false
end

module Context : sig 
    type t

    (** Create a new context *)
    val create_context : unit -> t

    (** Destroy all sockets initialised by the context. All connections will be closed *)
    val destroy_context : t -> unit
end = struct
    type t = {options : int}
    
    let create_context () = {options = 0}

(* TODO close all connections of sockets in the context *)
    let destroy_context t = ()
end

module rec Socket_base : sig
    type t

    (** Get the type of the socket *)
    val get_socket_type : t -> socket_type
    
    (** Get the metadata of the socket for handshake *)
    val get_metadata : t -> socket_metadata
    
    (** Get the security mechanism of the socket *)
    val get_mechanism : t -> mechanism_type

    (** Get the security credentials of the socket *)
    val get_security_data : t -> security_data
    
    (** Create a socket from the given context, mechanism and type *)
    val create_socket : Context.t -> ?mechanism:mechanism_type -> socket_type -> t
    
    (** Set username and password for PLAIN client *)
    val set_plain_credentials : t -> string -> string -> unit
    
    (** Set password list for PLAIN server *)
    val set_plain_user_list : t -> (string * string) list -> unit

    (** Set identity string of a socket if applicable *)
    val set_identity : t -> string -> unit
    
    (** Receive a msg from the underlying connections, according to the semantics of the socket type *)
    val recv : t -> string Lwt.t
    
    (** Send a msg to the underlying connections, according to the semantics of the socket type *)
    val send : t -> string -> unit

    val add_connection : t -> Connection.t ref -> unit
end = struct
    type socket_states =
        | NONE
        | Rep of {if_received : bool; 
                  last_received_connection_tag : string;}
        | Req
        | Dealer
        | Router
        | Pub
        | Sub
        | Xpub
        | Xsub
        | Push
        | Pull
        | Pair

    type t = {
        socket_type : socket_type;
        mutable metadata : socket_metadata;
        security_mechanism : mechanism_type;
        mutable security_info : security_data;
        mutable connections : (Connection.t ref) list;
        mutable socket_states : socket_states
    }

    let get_socket_type t = t.socket_type
    
    let get_metadata t = t.metadata 

    let get_mechanism t = t.security_mechanism
    
    let get_security_data t = t.security_info

    let create_socket context ?(mechanism=NULL) socket_type = 
        match socket_type with 
            | REP -> {
                socket_type = socket_type; 
                metadata = [("Socket-Type","REP")]; 
                security_mechanism = mechanism; 
                security_info = Null; 
                connections = [];
                socket_states = Rep({
                    if_received = false;
                    last_received_connection_tag = "";
                    });
                }
            | REQ -> {
                socket_type = socket_type; 
                metadata = [("Socket-Type","REQ")]; 
                security_mechanism = mechanism; 
                security_info = Null; 
                connections = [];
                socket_states = Req;
                }
            | DEALER -> {
                socket_type = socket_type; 
                metadata = [("Socket-Type","DEALER");("Identity","")]; 
                security_mechanism = mechanism; 
                security_info = Null; 
                connections = [];
                socket_states = Dealer;
                }
            | ROUTER -> {
                socket_type = socket_type; 
                metadata = [("Socket-Type","ROUTER")]; 
                security_mechanism = mechanism; 
                security_info = Null; 
                connections = [];
                socket_states = Router
                }
            | _ -> raise Not_Implemented
    
    let set_plain_credentials t name password = 
        if t.security_mechanism = PLAIN then t.security_info <- Plain_client(name, password)
        else raise Not_Able_To_Set_Credentials
    
    let set_plain_user_list t list = 
        if t.security_mechanism = PLAIN then t.security_info <- Plain_server(list)
        else raise Not_Able_To_Set_Credentials

    let set_identity t identity =
        if t.socket_type = DEALER then (
            let set (name, value) = if name = "Identity" then (name, identity) else (name, value) in
                t.metadata <- List.map set t.metadata
        )
        else ()

    let rec recv t = 
        match t.socket_type with 
        | REP -> (let state = t.socket_states in
                    match state with
                        | Rep({if_received = if_received;
                               last_received_connection_tag = tag;
                             }) -> (
                          (* Need to receive in a fair-queuing manner *)
                          (* Go through the list of connections and check buffer *)
                          if t.connections = [] then (
                              (*Logs.info (fun f -> f "Module Socket_base: No available connection\n"); *)
                              Lwt.pause() >>= fun () -> recv t)
                          else let rec check_buffer connections = match connections with
                            (* If no connection in the list, wait for an incoming connection *)
                            | [] -> Lwt.return None
                            | hd::tl -> (
                                        if Connection.get_stage (!hd) = TRAFFIC then
                                        (Logs.info (fun f -> f "Module Socket_base: Set-up connections available\n");
                                         let buffer = !(Connection.get_buffer (!hd)) in
                                            Lwt_stream.peek buffer >>= function
                                                | None -> (Logs.info (fun f -> f "Module Socket_base: Xonnection %s's buffer empty\n" (Connection.get_tag (!hd))); check_buffer tl)
(* TODO move the assignment downwards *)
                                                | Some(frame) -> t.socket_states <- Rep{if_received = true; 
                                                                                last_received_connection_tag = Connection.get_tag (!hd);
                                                                                };
                                                                 Lwt_stream.junk buffer >>= fun () ->
                                                                 Lwt.return (Some((frame, Connection.get_tag (!hd))))
                                        )
                                         else check_buffer tl
                                        )
                          in check_buffer t.connections >>= function
                                | None -> 
                                    (*Logs.info (fun f -> f "Module Socket_base: Connections' buffers are empty\n"); *)
                                    Lwt.pause() >>= fun () -> recv t
                                | Some((frame,tag)) -> (
                                    Logs.info (fun f -> f "Module Socket_base: Delimiter frame read from buffer: %s\n" (buffer_to_string (Frame.to_bytes frame)));
                                    (* Messages are separated by an empty delimiter *)
                                    if Frame.is_delimiter_frame frame then    
                                        (* Find the tagged connection *)
                                        let connection = List.find (fun x -> Connection.get_tag (!x) = tag) t.connections in
                                        (* Reconstruct message from the connection *)
                                        let rec frame_list list = 
                                            Lwt_stream.get (!(Connection.get_buffer (!connection))) >>= function
                                                | None -> raise Not_Implemented
                                                | Some(next_frame) -> if Frame.get_if_more next_frame then frame_list (list@[next_frame]) 
                                                                      else Lwt.return (list@[next_frame]) 
                                        in
                                            frame_list [] >>= fun x -> 
                                            (* Put the received connection at the end of the queue *)
                                            let rec reorder list accumu =
                                                match list with
                                                    | [] -> accumu @ [connection]
                                                    | hd::tl -> if Connection.get_tag (!hd) = tag then reorder tl accumu 
                                                                else reorder tl (accumu@[hd]) 
                                            in
                                            t.connections <- (reorder t.connections []);
(* TODO check error *)
                                            Lwt.return (Frame.splice_message_frames x)                
                                    else (* Protocol error, close the connection and try again *) 
                                        let connection = List.find (fun x -> Connection.get_tag (!x) = tag) t.connections in
                                            Connection.close (!connection);
(* TODO remove connection *)
                                            Lwt.pause() >>= fun () -> recv t
                                )
                        )
                        | _ -> raise (Internal_Error "Socket states mismatches with socket type")
                )
(* TODO implement other sockets' behaviour *)
        | _ -> raise Not_Implemented

    let send t msg = match t.socket_type with
        | REP -> (let state = t.socket_states in
                    match state with
                        | Rep({if_received = if_received; last_received_connection_tag = tag;}) -> (
                            if not if_received then raise (Incorrect_use_of_API "Need to receive from a REP before sending a message")
                            else let rec find connections = match connections with
                                    (* Discard the message if peer no longer connected *)
                                    | [] -> ()
                                    | hd::tl -> if tag = Connection.get_tag (!hd) then
                                                    if Connection.get_stage (!hd) = TRAFFIC then
                                                        (Connection.send (!hd) (Frame.delimiter_frame::(List.map (fun x -> Message.to_frame x) (Message.list_of_string msg)));
                                                        t.socket_states <- Rep({if_received = false; last_received_connection_tag = "";}))
                                                    else ()
                                                else find tl
                                in find t.connections
                        )
                        | _ -> raise (Internal_Error "Socket states mismatches with socket type")
                )
        | _ -> raise Not_Implemented

    let add_connection t connection = t.connections <- (t.connections@[connection])

end

and Security_mechanism : sig
    type t
    type action = Write of bytes | Continue | Close | Received_property of string * string | Ok
    
    (** Get the string description of the mechanism *)
    val get_name_string : t -> string
    
    (** Whether the socket is a PLAIN server (always false if mechanism is NULL) *)
    val get_as_server : t -> bool
    
    (** Whether the socket is a PLAIN client (always false if mechanism is NULL) *)
    val get_as_client : t -> bool
    
    (** Initialise a t from security mechanism data and socket metadata *)
    val init : security_data -> socket_metadata -> t
    
    (** If the socket is a PLAIN mechanism client, it needs to send the HELLO command first *)
    val client_first_message : t -> bytes
    
    (** FSM for handling the handshake *)
    val fsm : t -> Command.t -> t * action list
end = struct
    type state = START | START_SERVER | START_CLIENT | WELCOME | INITIATE | OK
    type action = Write of bytes | Continue | Close | Received_property of string * string | Ok

    type t = {
        mechanism_type : mechanism_type;
        socket_metadata : socket_metadata;
        state : state;
        data : security_data;
        as_server : bool;
        as_client : bool;
    }
   
    (* Start of helper functions *)
    (** Makes an ERROR command *)
    let error error_reason = 
        Frame.to_bytes (Command.to_frame (Command.make_command "ERROR" (Bytes.cat (Bytes.make 1 (Char.chr (String.length error_reason))) (Bytes.of_string error_reason))))

    (** Extracts metadata from a command *)
    let rec extract_metadata bytes = 
    let len = Bytes.length bytes in
        if len = 0 then []
        else 
            let name_length = Char.code (Bytes.get bytes 0) in
            let name = Bytes.sub_string bytes 1 name_length in
            let property_length = network_order_to_int (Bytes.sub bytes (name_length + 1) 4) in
            let property = Bytes.sub_string bytes (name_length + 1 + 4) property_length in
                (name, property)::(extract_metadata (Bytes.sub bytes (name_length + 1 + 4 + property_length) (len - (name_length + 1 + 4 + property_length))))

    (** Extracts username and password from HELLO *)
    let extract_username_password bytes =
    let username_length = Char.code (Bytes.get bytes 0) in
    let username = Bytes.sub bytes 1 username_length in
    let password_length = Char.code (Bytes.get bytes (1 + username_length)) in
    let password = Bytes.sub bytes (2 + username_length) password_length in
        (username, password)

    (** Makes a WELCOME command for a PLAIN server *)
    let welcome = 
        Frame.to_bytes (Command.to_frame (Command.make_command "WELCOME" Bytes.empty))

    (** Makes a HELLO command for a PLAIN client *)
    let hello username password = 
        Frame.to_bytes (Command.to_frame (Command.make_command "HELLO" (Bytes.concat Bytes.empty [Bytes.make 1 (Char.chr (String.length username));Bytes.of_string username;Bytes.make 1 (Char.chr (String.length password));Bytes.of_string password])))

    (** Makes a new handshake for NULL mechanism *)
    let new_handshake_null metadata = 
    let bytes_of_metadata (name, property) = (
        let name_length = String.length name in
        let property_length = String.length property in
            Bytes.cat
            (Bytes.cat (Bytes.make 1 (Char.chr name_length)) (Bytes.of_string name))
            (Bytes.cat (int_to_network_order property_length 4) (Bytes.of_string property))
    ) in
    let rec convert_metadata = function
        | [] -> Bytes.empty
        | hd::tl -> Bytes.cat (bytes_of_metadata hd) (convert_metadata tl)
    in
        Frame.to_bytes (Command.to_frame (Command.make_command "READY" (convert_metadata metadata)))

    (** Makes a metadata command (command = "INITIATE"/"READY") for PLAIN mechanism *)
    let metadata_command command metadata = 
    let bytes_of_metadata (name, property) = (
        let name_length = String.length name in
        let property_length = String.length property in
            Bytes.cat
            (Bytes.cat (Bytes.make 1 (Char.chr name_length)) (Bytes.of_string name))
            (Bytes.cat (int_to_network_order property_length 4) (Bytes.of_string property))
    ) in
    let rec convert_metadata = function
        | [] -> Bytes.empty
        | hd::tl -> Bytes.cat (bytes_of_metadata hd) (convert_metadata tl)
    in Frame.to_bytes (Command.to_frame (Command.make_command command (convert_metadata metadata)))

    (** Search the server's password list for the presented credentials *)
    let rec search (name : string) (password : string) list =
        match list with
            | [] -> false
            | (n, p)::tl -> (n = name && p = password) || (search name password tl)
    (* End of helper functions *)

    let get_name_string t = match t.mechanism_type with | NULL -> "NULL" | PLAIN -> "PLAIN"

    let init security_data socket_metadata = match security_data with
        | Null -> {mechanism_type = NULL; socket_metadata = socket_metadata; state = START; data = security_data; as_server = false; as_client = false;}
        | Plain_client(_) -> {mechanism_type = PLAIN; socket_metadata = socket_metadata; state = START_CLIENT; data = security_data; as_server = false; as_client = true;}
        | Plain_server(_) -> {mechanism_type = PLAIN; socket_metadata = socket_metadata; state = START_SERVER; data = security_data; as_server = true; as_client = false;}

    let client_first_message t =
        match t.mechanism_type with 
            | NULL -> Bytes.empty
            | PLAIN -> if t. as_client then 
                            match t.data with 
                                | Plain_client(u, p) -> hello u p
                                | _ -> raise (Internal_Error "Security mechanism mismatch")
                        else Bytes.empty

    let fsm t command =
    let name = Command.get_name command in
    let data = Command.get_data command in 
        match t.mechanism_type with 
            | NULL -> (match t.state with 
                        | START -> (match name with
                                    | "READY" -> ({t with state = OK}, (List.map (fun (name, property) -> Received_property(name, property)) (extract_metadata data)) @ [Write(new_handshake_null t.socket_metadata); Ok])
                                    | "ERROR" -> ({t with state = OK}, [Write(error "ERROR command received"); Close])
                                    | _ -> ({t with state = OK}, [Write(error "unknown command received"); Close]))
                        | _ -> raise Should_Not_Reach)
            | PLAIN -> (match t.state with 
                            |   START_SERVER -> 
                                if name = "HELLO" then (
                                    let (username, password) = extract_username_password data in
                                        match t.data with 
                                            | Plain_client(_) -> raise (Internal_Error "Server data expected")
                                            | Plain_server(list) ->  
                                                if search (Bytes.to_string username) (Bytes.to_string password) list then ({t with state = WELCOME}, [Write(welcome)])
                                                else ({t with state = OK}, [Write(error "Handshake error"); Close])
                                            | _ -> raise (Internal_Error "Security type mismatch")
                                ) else ({t with state = OK}, [Write(error "Handshake error"); Close])
                            | START_CLIENT -> if name = "WELCOME" then 
                                                ({t with state = INITIATE}, [Write(metadata_command "INITIATE" t.socket_metadata)])
                                              else raise (Internal_Error "Wrong credentials")
                            | WELCOME -> if name = "INITIATE" then 
                                            ({t with state = OK}, (List.map (fun (name, property) -> Received_property(name, property)) (extract_metadata data)) @ [Write(metadata_command "READY" t.socket_metadata); Ok]) 
                                        else ({t with state = OK}, [Write(error "Handshake error"); Close])
                            | INITIATE -> if name = "READY" then 
                                            ({t with state = OK}, (List.map (fun (name, property) -> Received_property(name, property)) (extract_metadata data)) @ [Ok]) 
                                        else ({t with state = OK}, [Write(error "Handshake error"); Close])
                            | _ -> raise Should_Not_Reach)

    let get_as_server t = t.as_server
    let get_as_client t = t.as_client
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
        | Send_bytes of bytes
        | Check_mechanism of string
        | Set_server of bool
        | Continue
        | Ok
        | Error of string

    (** Initialise a t from a security mechanism to be used *)
    val init : Security_mechanism.t -> t
    
    (** FSM call for handling a single event *)
    val fsm_single : t -> event -> t * action
    
    (** FSM call for handling a list of events. *)
    val fsm : t -> event list -> t * action list
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

    type t = {
        security : Security_mechanism.t;
        state : state;
    }

    type event =
        | Recv_sig of bytes
        | Recv_Vmajor of bytes
        | Recv_Vminor of bytes
        | Recv_Mechanism of bytes
        | Recv_as_server of bytes
        | Recv_filler
        | Init of string

    type action =
        | Send_bytes of bytes
        | Check_mechanism of string
        | Set_server of bool
        | Continue
        | Ok
        | Error of string

    type version = {major : bytes; minor : bytes}

    type greeting = { 
        signature  : bytes;
        version    : version;
        mechanism  : string;
        as_server  : bool;
        filler     : bytes
    }

    (* Start of helper functions *)
    (** Make the signature bytes *)
    let signature = 
    let s = Bytes.make 10 (Char.chr 0) in
        Bytes.set s 0 (Char.chr 255);
        Bytes.set s 9 (Char.chr 127);
        s

    (** The default version of this implementation is 3.0 (RFC 23/ZMTP) *)
    let version = {major = Bytes.make 1 (Char.chr 3); minor = Bytes.make 1 (Char.chr 0)}

    (** Pad the mechanism string to 20 bytes *)
    let pad_mechanism m =
    let b = Bytes.of_string m in
        if Bytes.length b < 20 then
            Bytes.cat b (Bytes.make (20 - Bytes.length b) (Char.chr 0))
        else
            b
    
    (** Get the actual mechanism from null padded string *)
    let trim_mechanism m =
    let len = ref (Bytes.length m) in
        while Bytes.get m (!len - 1) = Char.chr 0 do len := !len - 1 done;
        Bytes.sub m 0 (!len)

    (** Makes the filler bytes *)
    let filler = Bytes.make 31 (Char.chr 0)
    
    (** Generates a new greeting *)
    let new_greeting security = 
        Bytes.concat Bytes.empty [
            signature; 
            version.major; 
            version.minor; 
            pad_mechanism (Security_mechanism.get_name_string security); 
            if Security_mechanism.get_as_server security then (Bytes.make 1 (Char.chr 1)) else (Bytes.make 1 (Char.chr 0));
            filler
        ]
    (* End of helper functions *)
    
    let init security_t = {security = security_t; state = START}    

    let fsm_single t event = 
    match (t.state , event) with
        | (START, Recv_sig(b)) -> 
            if (Bytes.get b 0) = (Char.chr 255) && (Bytes.get b 9) = (Char.chr 127) 
            then ({t with state = SIGNATURE}, Send_bytes (new_greeting t.security)) 
            else ({t with state = ERROR}, Error("Protocol Signature not detected."))
        | (SIGNATURE, Recv_Vmajor(b)) ->
            if (Bytes.get b 0) = (Char.chr 3) 
            then ({t with state = VERSION_MAJOR}, Continue) 
            else ({t with state = ERROR}, Error("Version-major is not 3."))
        | (VERSION_MAJOR, Recv_Vminor(b)) ->
            if (Bytes.get b 0) = (Char.chr 0) 
            then ({t with state = VERSION_MINOR}, Continue) 
            else ({t with state = ERROR}, Error("Version-minor is not 0."))
        | (VERSION_MINOR, Recv_Mechanism(b)) ->
            ({t with state = MECHANISM}, Check_mechanism(Bytes.to_string(trim_mechanism b)))
        | (MECHANISM, Recv_as_server(b)) ->
            if (Bytes.get b 0) = (Char.chr 0)
            then ({t with state = AS_SERVER}, Set_server(false))
            else ({t with state = AS_SERVER}, Set_server(true))
        | (AS_SERVER, Recv_filler) -> ({t with state = SUCCESS}, Ok)
        | _ -> ({t with state = ERROR}, Error("Unexpected event."))

    let fsm t event_list =
    let rec fsm_accumulator t event_list action_list = 
        match event_list with
            | [] -> (match t.state with 
                        | ERROR -> ({t with state = ERROR}, [List.hd action_list])
                        | _ -> (t, List.rev action_list))
            | hd::tl -> match t.state with
                        | ERROR -> ({t with state = ERROR}, [List.hd action_list])
                        | _ -> let (new_state, action) = fsm_single t hd in
                                fsm_accumulator new_state tl (action::action_list)
    in fsm_accumulator t event_list []
end

and Connection : sig 
    type t
    type action =
    | Write of bytes
    | Continue
    | Close of string

    (** Create a new connection for socket with specified security mechanism *)
    val init : Socket_base.t -> Security_mechanism.t -> string -> t

    (** Get the unique tag used to identify the connection *)
    val get_tag : t -> string

    (** Get the read buffer of the connection *)
    val get_buffer : t -> Frame.t Lwt_stream.t ref

    (** Get the stage of the connection. It is considered usable if in TRAFFIC *)
    val get_stage : t -> connection_stage
    
    (** FSM for handing raw bytes transmission *)
    val fsm : t -> Bytes.t -> action list

    (** Send the list of frames to underlying connection *)
    val send : t -> Frame.t list -> unit

    (** Force close connection *)
    val close : t -> unit

    (** Set the the send buffer *)
    val set_send_buffer : t -> connection_buffer_object Lwt_stream.t -> unit

    (** Set the push function of the send buffer *)
    val set_send_pf : t -> (connection_buffer_object option -> unit) -> unit
end = struct 

    type action = | Write of bytes | Continue | Close of string
    
    type t = {
        tag : string;
        socket : Socket_base.t;
        mutable greeting_state : Greeting.t;
        mutable handshake_state : Security_mechanism.t;
        mutable stage : connection_stage;
        mutable expected_bytes_length : int;
        mutable incoming_as_server : bool;
        mutable incoming_socket_type : socket_type;
        mutable incoming_identity : string;
        read_buffer : Frame.t Lwt_stream.t ref;
        read_buffer_pf : (Frame.t option -> unit) ref;
        mutable send_buffer : connection_buffer_object Lwt_stream.t;
        mutable send_buffer_pf : connection_buffer_object option -> unit;
    }

    let init socket security_mechanism tag = 
    let stream, pf = Lwt_stream.create () in 
    let ref_stream = ref stream in 
    let ref_pf = ref pf in {
        tag = tag;
        socket = socket;
        greeting_state = Greeting.init security_mechanism;
        handshake_state = security_mechanism;
        stage = GREETING;
        expected_bytes_length = 64; (* A value of 0 means expecting a frame of any length; starting with expectint the whole greeting *)
        incoming_socket_type = REP;
        incoming_as_server = false;
        incoming_identity = "";
        read_buffer = ref_stream;
        read_buffer_pf = ref_pf;
        send_buffer = Lwt_stream.of_list [];
        send_buffer_pf = fun x -> ();
    }

    let get_tag t = t.tag

    let get_stage t = t.stage

    let get_buffer t = t.read_buffer

    let rec fsm t bytes = 
        match t.stage with
            | GREETING ->  (Logs.info (fun f -> f "Module Connection: Greeting -> FSM\n");
                            let len = Bytes.length bytes in
                            let rec convert greeting_action_list =
                                match greeting_action_list with
                                    | [] -> []
                                    | (hd::tl) ->
                                        match hd with 
                                            | Greeting.Send_bytes(b) -> (Write(b)::(convert tl))
                                            | Greeting.Set_server(b) -> t.incoming_as_server <- b; 
                                                                        if t.incoming_as_server && (Security_mechanism.get_as_server t.handshake_state) then [Close("Both ends cannot be servers")]
                                                                        else if (Security_mechanism.get_as_client t.handshake_state) && (not t.incoming_as_server) then [Close("Other end is not a server")]
                                                                        else convert tl
                                            (* Assume security mechanism is pre-set*)
                                            | Greeting.Check_mechanism(s) -> if s <> (Security_mechanism.get_name_string t.handshake_state) then [Close("Security Policy mismatch")]
                                                                             else convert tl
                                            | Greeting.Continue -> convert tl
                                            | Greeting.Ok -> Logs.info (fun f -> f "Module Connection: Greeting OK\n"); t.stage <- HANDSHAKE; 
                                                             if (Security_mechanism.get_as_client t.handshake_state) then Write(Security_mechanism.client_first_message t.handshake_state)::(convert tl)
                                                             else convert tl
                                            | Greeting.Error(s) -> [Close("Greeting FSM error: " ^ s)] 
                            in match len with
                                (* Hard code the length here. The greeting is either complete or split into 11 + 53 or 10 + 54 *)
                                (* Full greeting *)
                                | 64 -> let (state, action_list) = Greeting.fsm t.greeting_state 
                                            [Greeting.Recv_sig(Bytes.sub bytes 0 10); 
                                             Greeting.Recv_Vmajor(Bytes.sub bytes 10 1);
                                             Greeting.Recv_Vminor(Bytes.sub bytes 11 1);
                                             Greeting.Recv_Mechanism(Bytes.sub bytes 12 20);
                                             Greeting.Recv_as_server(Bytes.sub bytes 32 1);
                                             Greeting.Recv_filler  
                                            ] in
                                        let connection_action = convert action_list in 
                                        t.greeting_state <- state; t.expected_bytes_length <- 0; 
                                        connection_action
                                (* Signature + version major *)
                                | 11 -> let (state, action_list) = Greeting.fsm t.greeting_state [Greeting.Recv_sig(Bytes.sub bytes 0 10); Greeting.Recv_Vmajor(Bytes.sub bytes 10 1)] in
                                        t.greeting_state <- state; t.expected_bytes_length <- 53;
                                        convert action_list
                                (* Signature *)
                                | 10 -> let (state, action) = Greeting.fsm_single t.greeting_state (Greeting.Recv_sig(bytes)) in
                                        t.greeting_state <- state; t.expected_bytes_length <- 54;
                                        convert [action]
                                (* version minor + rest *)
                                | 53 -> let (state, action_list) = Greeting.fsm t.greeting_state 
                                            [Greeting.Recv_Vminor(Bytes.sub bytes 0 1);
                                             Greeting.Recv_Mechanism(Bytes.sub bytes 1 20);
                                             Greeting.Recv_as_server(Bytes.sub bytes 21 1);
                                             Greeting.Recv_filler   
                                            ] in
                                        let connection_action = convert action_list in 
                                        t.greeting_state <- state; t.expected_bytes_length <- 0;
                                        connection_action
                                (* version major + rest *)
                                | 54 -> let (state, action_list) = Greeting.fsm t.greeting_state 
                                            [Greeting.Recv_Vmajor(Bytes.sub bytes 0 1);
                                             Greeting.Recv_Vminor(Bytes.sub bytes 1 1);
                                             Greeting.Recv_Mechanism(Bytes.sub bytes 2 20);
                                             Greeting.Recv_as_server(Bytes.sub bytes 22 1);
                                             Greeting.Recv_filler   
                                            ] in
                                        let connection_action = convert action_list in 
                                        t.greeting_state <- state; t.expected_bytes_length <- 0;
                                        connection_action
                                | n -> if n < t.expected_bytes_length then [Close("Message too short")]
                                       else (let expected_length = t.expected_bytes_length in
                                            (* Handle greeting part *)
                                            let action_list_1 = fsm t (Bytes.sub bytes 0 expected_length) in
                                            (* Handle handshake part *)
                                            let action_list_2 = fsm t (Bytes.sub bytes expected_length (n - expected_length)) in
                                                (action_list_1 @ action_list_2)
                        ))
            | HANDSHAKE -> (Logs.info (fun f -> f "Module Connection: Handshake -> FSM\n");
                let command = Command.of_frame (Frame.of_bytes bytes) in
                            let (new_state, actions) = Security_mechanism.fsm t.handshake_state command in
                            let rec convert handshake_action_list = 
                                match handshake_action_list with 
                                    | [] -> []
                                    | (hd::tl) -> (match hd with
                                                    | Security_mechanism.Write(b) -> Write(b)::(convert tl)
                                                    | Security_mechanism.Continue -> Continue::(convert tl)
                                                    | Security_mechanism.Ok -> Logs.info (fun f -> f "Module Connection: Handshake OK\n"); t.stage <- TRAFFIC; convert tl
                                                    | Security_mechanism.Close -> [Close("Handshake FSM error")]
                                                    | Security_mechanism.Received_property(name, value) -> 
                                                        match name with
                                                            | "Socket-Type" -> (if if_valid_socket_pair (Socket_base.get_socket_type t.socket) (socket_type_from_string value) 
                                                                               then (t.incoming_socket_type <- (socket_type_from_string value); convert tl) else [Close("Socket type mismatch")])
                                                            | "Identity" -> t.incoming_identity <- value; convert tl
                                                            | _ -> Logs.info (fun f -> f "Module Connection: Ignore unknown identity %s\n" name); convert tl
                                                    )
                            in let actions = convert actions in
                            t.handshake_state <- new_state;
                            actions)
            | TRAFFIC -> Logs.info (fun f -> f "Module Connection: TRAFFIC -> FSM\n");
                        let frames = Frame.list_of_bytes bytes in
                            (* Put the received frames into the buffer *)
                            Logs.info (fun f -> f "Module Connection: %d frames enqueued\n" (List.length frames));
                            List.iter (fun x -> !(t.read_buffer_pf) (Some(x))) frames;
                            [Continue]
            | CLOSED -> [Close "Connection FSM error"]

    let close t = t.stage <- CLOSED; t.send_buffer_pf (Some(Command_close))

    let send t msg_list = 
        List.iter (fun x -> t.send_buffer_pf (Some(Data(Frame.to_bytes x)))) msg_list

    let set_send_buffer t buffer =
        t.send_buffer <- buffer

    let set_send_pf t pf =
        t.send_buffer_pf <- pf
end

module Connection_tcp (S: Mirage_stack_lwt.V4) = struct
    let listen s port socket =
    let rec read_and_print flow connection = 
        S.TCPV4.read flow >>= (function
        | Ok `Eof -> Logs.info (fun f -> f "Module Connection_tcp: Closing connection!");  Lwt.return_unit
        | Error e -> Logs.warn (fun f -> f "Module Connection_tcp: Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
        | Ok (`Data b) -> (Logs.info (fun f -> f "Module Connection_tcp: Read: %d bytes:\n%s" (Cstruct.len b)  (buffer_to_string (Cstruct.to_bytes b))); 
                                let action_list = Connection.fsm connection (Cstruct.to_bytes b) in
                                let rec act actions = 
                                    (match actions with
                                        | [] -> Lwt.pause() >>= fun () -> read_and_print flow connection
                                        | (hd::tl) -> 
                                        (match hd with
                                            | Connection.Write(b) -> Logs.info (fun f -> f "Module Connection_tcp: Connection FSM Write %d bytes\n%s\n" (Bytes.length b) (buffer_to_string b));
                                                            (S.TCPV4.write flow (Cstruct.of_bytes b) >>= function
                                                            | Error _ -> (Logs.warn (fun f -> f "Module Connection_tcp: Error writing data to established connection."); Lwt.return_unit)
                                                            | Ok () -> act tl)
                                            | Connection.Continue -> Logs.info (fun f -> f "Module Connection_tcp: Connection FSM Continue\n");
                                                            act tl
                                            | Connection.Close(s) -> Logs.info (fun f -> f "Module Connection_tcp: Connection FSM Close due to: %s\n" s);
                                                         Lwt.return_unit))
                                in act action_list))
    in let rec check_and_send_buffer buffer flow = (
        Lwt_stream.peek buffer >>= function
            | None -> Lwt.pause () >>= fun x -> check_and_send_buffer buffer flow
            | Some(data) -> match data with
                            | Data(b) -> (Logs.info (fun f -> f "Module Connection_tcp: Connection FSM Write %d bytes\n%s\n" (Bytes.length b) (buffer_to_string b));
                                S.TCPV4.write flow (Cstruct.of_bytes b) >>= function
                                   | Error _ -> (Logs.warn (fun f -> f "Module Connection_tcp: Error writing data to established connection."); Lwt.return_unit)
                                   | Ok () -> Lwt_stream.junk buffer >>= fun () -> check_and_send_buffer buffer flow)
                            | Command_close -> Logs.info (fun f -> f "Module Connection_tcp: Connection was instructed to close");
                                               S.TCPV4.close flow
    )
    in
        S.listen_tcpv4 s ~port (
            fun flow ->
                let dst, dst_port = S.TCPV4.dst flow in
                Logs.info (fun f -> f "Module Connection_tcp: New tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
                let connection = Connection.init socket (Security_mechanism.init (Socket_base.get_security_data socket) (Socket_base.get_metadata socket)) (tag_of_tcp_connection (Ipaddr.V4.to_string dst) dst_port) in
                let stream, pf = Lwt_stream.create () in 
                    Connection.set_send_pf connection pf;
                    Socket_base.add_connection socket (ref connection);
                    Lwt.join[read_and_print flow connection; check_and_send_buffer stream flow]
        );
        S.listen s

    let connect s addr port connection =
    let ipaddr = Ipaddr.V4.of_string_exn addr in
    let rec read_and_print flow connection = 
        S.TCPV4.read flow >>= (function
        | Ok `Eof -> Logs.info (fun f -> f "Module Connection_tcp: Closing connection!");  Lwt.return_unit
        | Error e -> Logs.warn (fun f -> f "Module Connection_tcp: Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
        | Ok (`Data b) -> (Logs.info (fun f -> f "Module Connection_tcp: Read: %d bytes:\n%s" (Cstruct.len b)  (buffer_to_string (Cstruct.to_bytes b))); 
                                let action_list = Connection.fsm connection (Cstruct.to_bytes b) in
                                let rec act actions = 
                                    (match actions with
                                        | [] -> Lwt.pause() >>= fun () -> read_and_print flow connection
                                        | (hd::tl) -> 
                                        (match hd with
                                            | Connection.Write(b) -> Logs.info (fun f -> f "Module Connection_tcp: Connection FSM Write %d bytes\n%s\n" (Bytes.length b) (buffer_to_string b));
                                                            (S.TCPV4.write flow (Cstruct.of_bytes b) >>= function
                                                            | Error _ -> (Logs.warn (fun f -> f "Module Connection_tcp: Error writing data to established connection."); Lwt.return_unit)
                                                            | Ok () -> act tl)
                                            | Connection.Continue -> Logs.info (fun f -> f "Module Connection_tcp: Connection FSM Continue\n");
                                                            act tl
                                            | Connection.Close(s) -> Logs.info (fun f -> f "Module Connection_tcp: Connection FSM Close due to: %s\n" s);
                                                         Lwt.return_unit))
                                in
                                    act action_list))
    in
        S.TCPV4.create_connection (S.tcpv4 s) (ipaddr, port) >>= function
            | Ok(f) -> read_and_print f connection
            | Error(e) -> Logs.warn (fun f -> f "Module Connection_tcp: Error establishing connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
end

module Socket_tcp (S : Mirage_stack_lwt.V4) : sig
    type t

    (** Create a socket from the given context, mechanism and type *)
    val create_socket : Context.t -> ?mechanism:mechanism_type -> socket_type -> t
    
    (** Set username and password for PLAIN client *)
    val set_plain_credentials : t -> string -> string -> unit
    
    (** Set password list for PLAIN server *)
    val set_plain_user_list : t -> (string * string) list -> unit

    (** Set identity string of a socket if applicable *)
    val set_identity : t -> string -> unit
    
    (** Receive a msg from the underlying connections, according to the  semantics of the socket type *)
    val recv : t -> string Lwt.t
    
    (** Send a msg to the underlying connections, according to the semantics of the socket type *)
    val send : t -> string -> unit
    
    (** Bind a local TCP port to the socket so the socket will accept incoming connections *)
    val bind : t -> int -> S.t -> unit
    
    (** Bind a connection to a remote TCP port to the socket *)
    val connect : t -> string -> int -> S.t -> unit
end = struct
    type transport_info = Tcp of string * int

    type t = {
        socket : Socket_base.t;
    }

    let create_socket context ?(mechanism = NULL) socket_type = {
        socket = Socket_base.create_socket context ~mechanism socket_type;
    }
        
    let set_plain_credentials t username password = 
        Socket_base.set_plain_credentials t.socket username password

    let set_plain_user_list t list = 
        Socket_base.set_plain_user_list t.socket list

    let set_identity t identity =
        Socket_base.set_identity t.socket identity
    
    let recv t =
        Socket_base.recv t.socket

    let send t msg =
        Socket_base.send t.socket msg
    
    let bind t port s = 
    let module C_tcp = Connection_tcp (S) in
        Lwt.async (fun () -> C_tcp.listen s port t.socket)

    let connect t ipaddr port s = 
    let module C_tcp = Connection_tcp (S) in
    let connection = Connection.init t.socket (Security_mechanism.init (Socket_base.get_security_data t.socket) (Socket_base.get_metadata t.socket)) (tag_of_tcp_connection ipaddr port) in
        Socket_base.add_connection t.socket (ref connection);
        raise Not_Implemented
end