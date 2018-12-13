open Lwt.Infix

exception Not_Implemented
exception Should_Not_Reach
exception Frame_Not_Complete
exception Socket_Name_Not_Recognised
exception Not_Able_To_Set_Credentials
exception Internal_Error of string

type socket_type = REQ | REP | DEALER | ROUTER | PUB | XPUB | SUB | XSUB | PUSH | PULL | PAIR
(* CURVE not implemented *)
type mechanism_type = NULL | PLAIN

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

let rec network_order_to_int bytes = 
    let length = Bytes.length bytes in
        if length = 1 then Char.code (Bytes.get bytes 0)
        else (Char.code (Bytes.get bytes (length - 1))) + (network_order_to_int (Bytes.sub bytes 0 (length - 1))) * 256

let int_to_network_order n length =
    Bytes.init length (fun i -> (Char.chr (n lsr (8 * (length - i - 1)) land 255)))

let buffer_to_string data = 
let content = ref [] in
    Bytes.iter (fun b -> content := Char.code b :: !content) data;
    String.concat " "  (List.map (fun x -> if (x >= 65 && x <= 90) || (x >= 97 && x <= 122) then String.make 1 (Char.chr x) else string_of_int x)  (List.rev !content))

module Frame : sig
    type t

    (* make_frame body ifMore ifCommand*)
    val make_frame : bytes -> bool -> bool -> t
    val to_bytes : t -> bytes
    val of_bytes : bytes -> t
    val if_more : t -> bool
    val if_command : t -> bool
    val body : t -> bytes
end = struct
    type t = { flag : char; size : int; body : bytes}

    (** TODO network bytes order *)
    let size_to_bytes size = 
        if size < 255 then
            Bytes.make 1 (Char.chr size)
        else
            Bytes.init 8 (fun i -> Char.chr ((size land (255 lsl (i - 1) * 8)) lsr ((i - 1) * 8)))

    let make_frame body ifMore ifCommand = 
        let f = ref 0 in
        let len = Bytes.length body in
            if ifMore then f := !f + 1;
            if ifCommand then f := !f + 4;
            if len > 255 then f := !f + 2;
            {flag = (Char.chr (!f)); size = len; body}

    let to_bytes t =
        Bytes.concat Bytes.empty [Bytes.make 1 t.flag; size_to_bytes t.size; t.body]

    let of_bytes bytes = 
    let flag = Char.code (Bytes.get bytes 0) in
    let ifLong = (flag land 2) = 2 in
        if ifLong then
            (* long-size *)
            raise Not_Implemented
        else
            (* short-size *)
            let length = Char.code (Bytes.get bytes 1) in
                (* check length *)
                if (Bytes.length bytes) <> length + 2 then raise Frame_Not_Complete;
                {flag = Char.chr flag; size = length; body = Bytes.sub bytes 2 length}
    
    let if_more t = (Char.code(t.flag) land 1) = 1
    let if_command t = (Char.code(t.flag) land 4) = 4
    let body t = t.body
end

module Command : sig
    type t
    val to_frame : t -> Frame.t
    val get_name : t -> string
    val get_data : t -> bytes
    val of_frame : Frame.t -> t
    val make_command : string -> bytes -> t
end = struct
    type t = { name : string; data : bytes }
    let to_frame t = Frame.make_frame (Bytes.concat Bytes.empty [(Bytes.make 1 (Char.chr (String.length t.name))); Bytes.of_string t.name; t.data]) false true 
    let get_name t = t.name
    let get_data t = t.data
    
    let of_frame frame = 
    let data = Frame.body frame in
    let name_length = Char.code (Bytes.get data 0) in
        {name = Bytes.sub_string data 1 name_length; data = Bytes.sub data (name_length + 1) ((Bytes.length data) - 1 - name_length)}

    let make_command name data =
        {name = name; data = data}
end

module type Socket_type = sig
    type t
    val socket_type : socket_type
    val metadata : (string * string) list
end

module REP_socket : Socket_type = struct
    type t
    let socket_type = REP
    let metadata = [("Socket-Type","REP")]
end

module REQ : Socket_type = struct
    type t
    let socket_type = REQ
    let metadata = [("Socket-Type","REQ")]
end

module DEALER : Socket_type = struct
    type t
    let socket_type = DEALER
    let metadata = [("Socket-Type","DEALER");("Identity","")]
end

module ROUTER : Socket_type = struct
    type t
    let socket_type = ROUTER
    let metadata = [("Socket-Type","ROUTER")]
end

module type Connection = sig
    type stage
    type t
    type action =
    | Write of bytes
    | Continue
    | Close of string
    val connection_fsm : t -> Bytes.t -> t * action list
    val new_connection : unit -> t
end

module type Security_Mechanism = sig
    type t
    type action = Write of bytes | Continue | Close | Received_property of string * string | Ok
    val name : string
    val fsm : t -> Command.t -> t * action list
    val init_state : t
    val as_server : bool
    val as_client : bool
    val client_first_message : bytes
end

module NULL_mechanism (S: Socket_type) : Security_Mechanism = struct
    type t = START | OK
    type action = Write of bytes | Continue | Close | Received_property of string * string | Ok
    let name = "NULL"
    let init_state = START
    let as_server = false
    let as_client = false
    let client_first_message = Bytes.empty

    let rec extract_metadata bytes = 
    let len = Bytes.length bytes in
        if len = 0 then []
        else 
            let name_length = Char.code (Bytes.get bytes 0) in
            let name = Bytes.sub_string bytes 1 name_length in
            let property_length = network_order_to_int (Bytes.sub bytes (name_length + 1) 4) in
            let property = Bytes.sub_string bytes (name_length + 1 + 4) property_length in
                (name, property)::(extract_metadata (Bytes.sub bytes (name_length + 1 + 4 + property_length) (len - (name_length + 1 + 4 + property_length))))

    let new_handshake = 
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
        Frame.to_bytes (Command.to_frame (Command.make_command "READY" (convert_metadata S.metadata)))

    let error error_reason = 
        Frame.to_bytes (Command.to_frame (Command.make_command "ERROR" (Bytes.cat (Bytes.make 1 (Char.chr (String.length error_reason))) (Bytes.of_string error_reason))))

    let fsm (state:t) command =
    let name = Command.get_name command in
    let data = Command.get_data command in 
        match state with 
            | START -> (match name with
                        | "READY" -> (OK, (List.map (fun (name, property) -> Received_property(name, property)) (extract_metadata data)) @ [Write(new_handshake); Ok])
                        | "ERROR" -> (OK, [Write(error "ERROR command received"); Close])
                        | _ -> (OK, [Write(error "unknown command received"); Close]))
            | _ -> raise Should_Not_Reach
end

module type PLAIN_mechanism_data = sig
    type data = Client of string * string | Server of (string * string) list
    val as_server : bool
    val data : data
end

module PLAIN_mechanism (S : Socket_type) (D : PLAIN_mechanism_data) : Security_Mechanism = struct
    type t = START_SERVER | START_CLIENT | WELCOME | INITIATE | OK
    type action = Write of bytes | Continue | Close | Received_property of string * string | Ok
    let name = "PLAIN"

    let init_state = 
        if D.as_server then START_SERVER else START_CLIENT

    let as_server = D.as_server
    let as_client = not D.as_server

    let client_first_message = 
        if as_client then raise Not_Implemented
        else Bytes.empty

    let error error_reason = 
        Frame.to_bytes (Command.to_frame (Command.make_command "ERROR" (Bytes.cat (Bytes.make 1 (Char.chr (String.length error_reason))) (Bytes.of_string error_reason))))

    let rec extract_metadata bytes = 
    let len = Bytes.length bytes in
        if len = 0 then []
        else 
            let name_length = Char.code (Bytes.get bytes 0) in
            let name = Bytes.sub_string bytes 1 name_length in
            let property_length = network_order_to_int (Bytes.sub bytes (name_length + 1) 4) in
            let property = Bytes.sub_string bytes (name_length + 1 + 4) property_length in
                (name, property)::(extract_metadata (Bytes.sub bytes (name_length + 1 + 4 + property_length) (len - (name_length + 1 + 4 + property_length))))

    let rec search name password list =
        match list with
            | [] -> false
            | (n, p)::tl -> (n = name && p = password) || (search name password tl)

    let welcome = 
        Frame.to_bytes (Command.to_frame (Command.make_command "WELCOME" Bytes.empty))

    let metadata_command command = 
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
        Frame.to_bytes (Command.to_frame (Command.make_command command (convert_metadata S.metadata)))

    let fsm state command =
    let name = Command.get_name command in
    let data = Command.get_data command in 
        match state with 
            | START_SERVER -> if name = "HELLO" then (
                                let meta_data = extract_metadata data in
                                match meta_data with | [] -> (OK, [Write(error "Handshake error"); Close])
                                | (_, name)::tl -> match tl with | [] -> (OK, [Write(error "Handshake error"); Close])
                                | (_, password)::tl -> match D.data with | Client(_) -> raise (Internal_Error "Server data expected")
                                | Server(list) ->  
                                    if search name password list then (WELCOME, [Write(welcome)])
                                    else (OK, [Write(error "Handshake error"); Close])
                                ) else (OK, [Write(error "Handshake error"); Close])
            | START_CLIENT -> if name = "WELCOME" then 
                                (INITIATE, [Write(metadata_command "INITIATE")])
                              else raise (Internal_Error "Wrong credentials")
            | WELCOME -> if name = "INITIATE" then 
                            (OK, (List.map (fun (name, property) -> Received_property(name, property)) (extract_metadata data)) @ [Write(metadata_command "READY"); Ok]) 
                        else (OK, [Write(error "Handshake error"); Close])
            | INITIATE -> if name = "READY" then 
                            (OK, (List.map (fun (name, property) -> Received_property(name, property)) (extract_metadata data)) @ [Ok]) 
                        else (OK, [Write(error "Handshake error"); Close])
            | _ -> raise Should_Not_Reach

end

module type Greeting = sig
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
    val init_state : t
    val handle : t * event -> t * action
    val handle_list : t * event list -> action list -> t * action list
end 

(* TODO: as_server determined by socket_type?*)
module New_Greeting (M : Security_Mechanism) : Greeting = struct
    type t =
        | START
        | SIGNATURE
        | VERSION_MAJOR
        | VERSION_MINOR
        | MECHANISM
        | AS_SERVER
        | SUCCESS
        | ERROR
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
    type version = { major : bytes; minor : bytes}

    type greeting = { 
        signature  : bytes;
        version    : version;
        mechanism  : string;
        as_server  : bool;
        filler     : bytes
    }

    let signature = 
    let s = Bytes.make 10 (Char.chr 0) in
        Bytes.set s 0 (Char.chr 255);
        Bytes.set s 9 (Char.chr 127);
        s

    let version = {major = Bytes.make 1 (Char.chr 3); minor = Bytes.make 1 (Char.chr 0)}

    let filler = Bytes.make 31 (Char.chr 0)

    let pad_mechanism m =
    let b = Bytes.of_string m in
        if Bytes.length b < 20 then
            Bytes.cat b (Bytes.make (20 - Bytes.length b) (Char.chr 0))
        else
            b

    let trim_mechanism m =
    let len = ref (Bytes.length m) in
        while Bytes.get m (!len - 1) = Char.chr 0 do len := !len - 1 done;
        Bytes.sub m 0 (!len)

    let to_bytes g = 
        Bytes.concat Bytes.empty [g.signature; g.version.major; g.version.minor; pad_mechanism g.mechanism; if g.as_server then (Bytes.make 1 (Char.chr 1)) else (Bytes.make 1 (Char.chr 0));g.filler]

    let new_greeting mechanism = {signature; version; mechanism; as_server = M.as_server; filler}

    let init_state = START

    let handle (current_state, event) = 
    match (current_state , event) with
        | (START, Recv_sig(b)) -> 
            if (Bytes.get b 0) = (Char.chr 255) && (Bytes.get b 9) = (Char.chr 127) 
            then (SIGNATURE, Send_bytes (new_greeting M.name |> to_bytes)) 
            else (ERROR, Error("Protocol Signature not detected."))
        | (SIGNATURE, Recv_Vmajor(b)) ->
            if (Bytes.get b 0) = (Char.chr 3) 
            then (VERSION_MAJOR, Continue) 
            else (ERROR, Error("Version-major is not 3."))
        | (VERSION_MAJOR, Recv_Vminor(b)) ->
            if (Bytes.get b 0) = (Char.chr 0) 
            then (VERSION_MINOR, Continue) 
            else (ERROR, Error("Version-minor is not 0."))
        | (VERSION_MINOR, Recv_Mechanism(b)) ->
            (MECHANISM, Check_mechanism(Bytes.to_string(trim_mechanism b)))
        | (MECHANISM, Recv_as_server(b)) ->
            if (Bytes.get b 0) = (Char.chr 0)
            then (AS_SERVER, Set_server(false))
            else (AS_SERVER, Set_server(true))
        | (AS_SERVER, Recv_filler) -> (SUCCESS, Ok)
        | _ -> (ERROR, Error("Unexpected event."))

    let rec handle_list (current_state, event_list) action_list =
        match event_list with
            | [] -> (match current_state with 
                        | ERROR -> (ERROR, [List.hd action_list])
                        | _ -> (current_state, List.rev action_list))
            | hd::tl -> match current_state with
                        | ERROR -> (ERROR, [List.hd action_list])
                        | _ -> let (new_state, action) = handle (current_state, hd) in
                            handle_list (new_state, tl) (action::action_list)
end

module New_Connection (S : Socket_type) (M : Security_Mechanism) : Connection = struct 
    type stage = 
    | GREETING
    | HANDSHAKE
    | TRAFFIC
    | ERROR

    type action =
    | Write of bytes
    | Continue
    | Close of string

    module Greeting = New_Greeting (M)
    
    type t = {
        mutable stage : stage;
        mutable as_server : bool;
        security_policy : string;
        expected_bytes_length : int;
        greeting_state : Greeting.t;
        handshake_state : M.t;
        mutable incoming_socket : socket_type;
        mutable incoming_identity : string;
    }

    let new_connection () = {
        stage = GREETING;
        as_server = false;
        expected_bytes_length = 64; (* A value of 0 means expecting a frame of any length *)
        greeting_state = Greeting.init_state;
        handshake_state = M.init_state;
        (* Set custom policy *)
        security_policy = M.name;
        incoming_socket = REP;
        incoming_identity = ""
    }

    let rec connection_fsm t bytes = 
        match (t.stage) with
            | GREETING ->  (let len = Bytes.length bytes in
                            let rec convert greeting_action_list =
                                match greeting_action_list with
                                    | [] -> []
                                    | (hd::tl) ->
                                        match hd with 
                                            | Greeting.Send_bytes(b) -> (Write(b)::(convert tl))
                                            | Greeting.Set_server(b) -> t.as_server <- b; 
                                                                        if t.as_server && M.as_server then raise (Internal_Error "Both ends cannot be servers")
                                                                        else if M.as_client && not t.as_server then raise (Internal_Error "Other end is not a server")
                                                                        else convert tl
                                            (* Assume security mechanism is pre-set*)
                                            | Greeting.Check_mechanism(s) -> if s <> t.security_policy then [Close("Security Policy mismatch")]
                                                                             else convert tl
                                            | Greeting.Continue -> convert tl
                                            | Greeting.Ok -> Logs.info (fun f -> f "Greeting OK\n"); t.stage <- HANDSHAKE; 
                                                             if M.as_client then Write(M.client_first_message)::(convert tl)
                                                             else convert tl
                                            | Greeting.Error(s) -> [Close("Greeting FSM error" ^ s)] in
                            (* Hard code the length here. The greeting is either complete or split into 11 + 53 or 10 + 54*)
                            match len with
                                (* Full greeting *)
                                | 64 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                            [Greeting.Recv_sig(Bytes.sub bytes 0 10); 
                                             Greeting.Recv_Vmajor(Bytes.sub bytes 10 1);
                                             Greeting.Recv_Vminor(Bytes.sub bytes 11 1);
                                             Greeting.Recv_Mechanism(Bytes.sub bytes 12 20);
                                             Greeting.Recv_as_server(Bytes.sub bytes 32 1);
                                             Greeting.Recv_filler  
                                            ]) [] in
                                        let connection_action = convert action_list in 
                                        ({t with greeting_state = state; expected_bytes_length = 0}, connection_action)
                                (* Signature + version major *)
                                | 11 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, [Greeting.Recv_sig(Bytes.sub bytes 0 10); Greeting.Recv_Vmajor(Bytes.sub bytes 10 1)]) [] in
                                        ({t with greeting_state = state; expected_bytes_length = 53}, convert action_list)
                                (* Signature *)
                                | 10 -> let (state, action) = Greeting.handle (t.greeting_state, Greeting.Recv_sig(bytes)) in
                                        ({t with greeting_state = state; expected_bytes_length = 54}, convert [action])
                                (* version minor + rest *)
                                | 53 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                            [Greeting.Recv_Vminor(Bytes.sub bytes 0 1);
                                             Greeting.Recv_Mechanism(Bytes.sub bytes 1 20);
                                             Greeting.Recv_as_server(Bytes.sub bytes 21 1);
                                             Greeting.Recv_filler   
                                            ]) [] in
                                        let connection_action = convert action_list in 
                                        ({t with greeting_state = state; expected_bytes_length = 0}, connection_action)
                                (* version major + rest *)
                                | 54 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                            [Greeting.Recv_Vmajor(Bytes.sub bytes 0 1);
                                             Greeting.Recv_Vminor(Bytes.sub bytes 1 1);
                                             Greeting.Recv_Mechanism(Bytes.sub bytes 2 20);
                                             Greeting.Recv_as_server(Bytes.sub bytes 22 1);
                                             Greeting.Recv_filler   
                                            ]) [] in
                                        let connection_action = convert action_list in 
                                        ({t with greeting_state = state; expected_bytes_length = 0}, connection_action)
                                | n -> if n < t.expected_bytes_length then (t, [Close("Message too short")])
                                       else (let expected_length = t.expected_bytes_length in
                                            (* Handle greeting part *)
                                            let (new_t_1, action_list_1) = connection_fsm t (Bytes.sub bytes 0 expected_length) in
                                            (* Handle handshake part *)
                                            let (new_t_2, action_list_2) = connection_fsm new_t_1 (Bytes.sub bytes expected_length (n - expected_length)) in
                                                (new_t_2, action_list_1 @ action_list_2))
                        )
            | HANDSHAKE -> (let command = Command.of_frame (Frame.of_bytes bytes) in
                            let (new_state, action) = M.fsm t.handshake_state command in
                            let rec convert handshake_action_list = 
                                match handshake_action_list with 
                                    | [] -> []
                                    | (hd::tl) -> (match hd with
                                                    | M.Write(b) -> Write(b)::(convert tl)
                                                    | M.Continue -> Continue::(convert tl)
                                                    | M.Ok -> Logs.info (fun f -> f "Handshake OK\n"); t.stage <- TRAFFIC; convert tl
                                                    | M.Close -> [Close("Handshake FSM error")]
                                                    | M.Received_property(name, value) -> 
                                                        match name with
                                                            | "Socket-Type" -> (if if_valid_socket_pair S.socket_type (socket_type_from_string value) 
                                                                               then (t.incoming_socket <- (socket_type_from_string value); convert tl) else [Close("Socket type mismatch")])
                                                            | "Identity" -> t.incoming_identity <- value; convert tl
                                                            | _ -> Logs.info (fun f -> f "Ignore unknown identity %s\n" name); convert tl
                                                    )
                            in
                            ({t with handshake_state = new_state}, convert action))
            | TRAFFIC -> (t, [])
            | ERROR -> (t, [])

    (* 
    let send msg = 
        push_function msg
    
    *)        

end

module Connection_tcp (S: Mirage_stack_lwt.V4) (C: Connection) = struct
    let listen s port socket =
    let rec read_and_print flow t = 
        S.TCPV4.read flow >>= (function
        | Ok `Eof -> Logs.info (fun f -> f "Closing connection!");  Lwt.return_unit
        | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
        | Ok (`Data b) -> (Logs.info (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b)  (buffer_to_string (Cstruct.to_bytes b))); 
                                let (new_t, action_list) = C.connection_fsm t (Cstruct.to_bytes b) in
                                let rec act actions = 
                                    (match actions with
                                        | [] -> read_and_print flow new_t
                                        | (hd::tl) -> 
                                        (match hd with
                                            | C.Write(b) -> Logs.info (fun f -> f "Connection FSM Write %d bytes\n%s\n" (Bytes.length b) (buffer_to_string b));
                                                            (S.TCPV4.write flow (Cstruct.of_bytes b) >>= function
                                                            | Error _ -> (Logs.warn (fun f -> f "Error writing data to established connection."); Lwt.return_unit)
                                                            | Ok () -> act tl)
                                            | C.Continue -> Logs.info (fun f -> f "Connection FSM Continue\n");
                                                            act tl
                                            | C.Close(s) -> Logs.info (fun f -> f "Connection FSM Close due to: %s\n" s);
                                                         Lwt.return_unit))
                                in
                                    act action_list))
    in
        S.listen_tcpv4 s ~port (
            fun flow ->
                let dst, dst_port = S.TCPV4.dst flow in
                Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
                let stream, push_function = Lwt_stream.create () in 
                (* create a stream and pass the ref to the socket
                
                
                Connection.add_mailbox socket (stream, push_function);
                let process_stream stream = Lwt_stream.get stream >>= function
                    | Some (action) -> (match action with 
                                            | Write(b) -> Logs.info (fun f -> f "Connection async Write %d bytes\n%s\n" (Bytes.length b) (buffer_to_string b));
                                                            (S.TCPV4.write flow (Cstruct.of_bytes b) >>= function
                                                            | Error _ -> (Logs.warn (fun f -> f "Error writing data to established connection."); Lwt.return_unit)
                                                            | Ok () -> act tl)
                                            | Close -> S.disconnect s)
                    | None -> process_stream stream
                lwt.join [read_and_print; check stream]
                *)
                (* *)
                read_and_print flow (C.new_connection ())
        );
        S.listen s

    let connect s addr port =
    let ipaddr = Ipaddr.V4.of_string_exn addr in
    let rec read_and_print flow t = 
        S.TCPV4.read flow >>= (function
        | Ok `Eof -> Logs.info (fun f -> f "Closing connection!");  Lwt.return_unit
        | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
        | Ok (`Data b) -> (Logs.info (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b)  (buffer_to_string (Cstruct.to_bytes b))); 
                                let (new_t, action_list) = C.connection_fsm t (Cstruct.to_bytes b) in
                                let rec act actions = 
                                    (match actions with
                                        | [] -> read_and_print flow new_t
                                        | (hd::tl) -> 
                                        (match hd with
                                            | C.Write(b) -> Logs.info (fun f -> f "Connection FSM Write %d bytes\n%s\n" (Bytes.length b) (buffer_to_string b));
                                                            (S.TCPV4.write flow (Cstruct.of_bytes b) >>= function
                                                            | Error _ -> (Logs.warn (fun f -> f "Error writing data to established connection."); Lwt.return_unit)
                                                            | Ok () -> act tl)
                                            | C.Continue -> Logs.info (fun f -> f "Connection FSM Continue\n");
                                                            act tl
                                            | C.Close(s) -> Logs.info (fun f -> f "Connection FSM Close due to: %s\n" s);
                                                         Lwt.return_unit))
                                in
                                    act action_list))
    in
        S.TCPV4.create_connection (S.tcpv4 s) (ipaddr, port) >>= function
            | Ok(f) -> read_and_print f (C.new_connection ())
            | Error(e) -> Logs.warn (fun f -> f "Error establishing connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
end

module Context = struct
    type t = {options : int}
    let create_context () = {options = 0}
end

module type Socket = sig
    type t
    val create_socket : Context.t -> ?mechanism:mechanism_type -> socket_type -> t
    val set_plain_credentials : t -> string -> string -> t
    val set_plain_user_list : t -> (string * string) list -> t
    val recv : t -> string
    val send : t -> string -> unit
end

module Socket_tcp (S : Mirage_stack_lwt.V4) : Socket = struct
    type transport_info = Tcp of string * int
    type security_info = NA | PLAIN_CLIENT of string * string | PLAIN_SERVER of (string * string) list
    type t = {
        socket_type : socket_type;
        mutable transport_info : transport_info;
        security_mechanism : mechanism_type;
        security_info : security_info

    }

    let default_t = {
        socket_type = REP;
        transport_info = Tcp("", 0);
        security_mechanism = NULL;
        security_info = NA
    }

    let create_socket context ?(mechanism = NULL) socket_type  =
        match socket_type with
            | REP -> {default_t with socket_type = REP; security_mechanism = mechanism}
            | REQ -> {default_t with socket_type = REQ; security_mechanism = mechanism}
            | DEALER -> {default_t with socket_type = DEALER; security_mechanism = mechanism}
            | ROUTER -> {default_t with socket_type = ROUTER; security_mechanism = mechanism}
            | PUB  -> {default_t with socket_type = PUB; security_mechanism = mechanism}
            | XPUB -> {default_t with socket_type = XPUB; security_mechanism = mechanism}
            | SUB -> {default_t with socket_type = SUB; security_mechanism = mechanism}
            | XSUB -> {default_t with socket_type = XSUB; security_mechanism = mechanism}
            | PUSH -> {default_t with socket_type = PUSH; security_mechanism = mechanism}
            | PULL -> {default_t with socket_type = PULL; security_mechanism = mechanism}
            | PAIR -> {default_t with socket_type = PAIR; security_mechanism = mechanism}
    
    let bind t port s = 
    let module C = 
        (val (match t.socket_type with
                | REP ->(match t.security_mechanism with
                            | NULL -> (module New_Connection (REP_socket) (NULL_mechanism (REP_socket)) : Connection)
                            | PLAIN -> match t.security_info with
                                         | PLAIN_CLIENT(name, password) -> (module New_Connection (REP_socket) (PLAIN_mechanism (REP_socket) (
                                                                            struct 
                                                                                type data = Client of string * string | Server of (string * string) list
                                                                                let as_server = false
                                                                                let data = Client(name, password)
                                                                            end 
                                         )) : Connection)
                                         | PLAIN_SERVER(list) -> (module New_Connection (REP_socket) (PLAIN_mechanism (REP_socket) (
                                                                            struct 
                                                                                type data = Client of string * string | Server of (string * string) list
                                                                                let as_server = true
                                                                                let data = Server(list)
                                                                            end )) : Connection)
                                         | _ -> raise (Internal_Error "security mechanism mismatch")
                        )
                | _ -> raise Not_Implemented) 
            : Connection) in
    let module C_tcp = Connection_tcp (S) (C) in
        Lwt.async (fun () -> C_tcp.listen s port s)

    let connect t ipaddr port s = raise Not_Implemented

    let set_plain_credentials t name password = 
        if t.security_mechanism = PLAIN then {t with security_info = PLAIN_CLIENT(name, password)}
        else raise Not_Able_To_Set_Credentials
    
    let set_plain_user_list t list = 
        if t.security_mechanism = PLAIN then {t with security_info = PLAIN_SERVER(list)}
        else raise Not_Able_To_Set_Credentials

    let recv t = raise Not_Implemented

    let send t msg = raise Not_Implemented
end

module type Traffic = sig
    type t
end


