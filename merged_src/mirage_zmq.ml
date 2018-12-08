open Lwt.Infix

exception Not_Implemented
exception Frame_Not_Complete
type socket_type = REQ | REP | DEALER | ROUTER | PUB | XPUB | SUB | XSUB | PUSH | PULL | PAIR
type mechanism_type = NULL | PLAIN

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

module Frame : sig
    type t
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
        if size > 255 then
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
    let (ifCommand, ifLong, ifMore) = ((flag land 4) = 4, (flag land 2) = 2, (flag land 1) = 1) in
        if ifLong then
            (* long-size *)
            raise Not_Implemented
        else
            (* short-size *)
            let length = Char.code (Bytes.get bytes 1) in
                (* check length *)
                if (Bytes.length bytes) <> length + 2 then raise Frame_Not_Complete;
                {flag = Char.chr flag; size = length; body = Bytes.sub bytes 2 (length - 2)}
    
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
end = struct
    type t = { name : string; data : bytes }
    let to_frame t = Frame.make_frame (Bytes.concat Bytes.empty [Bytes.of_string t.name; t.data]) false true 
    let get_name t = t.name
    let get_data t = t.data
    
    let of_frame frame = 
    let data = Frame.body frame in
    let name_length = Char.code (Bytes.get data 0) in
        {name = Bytes.sub_string data 1 name_length; data = Bytes.sub data (name_length + 1) ((Bytes.length data) - 1 - name_length)}
end

module type Socket_type = sig
    val name : string
end

module REP_socket : Socket_type = struct
    let name = "REP"
end

module REQ : Socket_type = struct
    let name = "REQ"
end

module DEALER : Socket_type = struct
    let name = "DEALER"
end

module ROUTER : Socket_type = struct
    let name = "ROUTER"
end
module type Connection = sig
    type stage
    type t
    type action =
    | Write of bytes
    | Continue
    | Close
    val connection_fsm : t -> Bytes.t -> t * action list
    val new_connection : unit -> t
end

module type Security_Mechanism = sig
    type t
    type action = Write of bytes | Continue | Close
    val name : string
    val fsm : t -> Command.t -> t * action
    val init_state : t
end

module NULL_mechanism : Security_Mechanism = struct
    type t = START | READY | OK
    type action = Write of bytes | Continue | Close
    let name = "NULL"

    let init_state = START

    let fsm (state:t) command =
    let name = Command.get_name command in
    let data = Command.get_data command in (state, Continue)
end

module PLAIN_mechanism : Security_Mechanism = struct
    type t = START | READY | OK
    type action = Write of bytes | Continue | Close
    let name = "PLAIN"
    let init_state = START
    let fsm (state:t) command = raise Not_Implemented

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

    let new_greeting mechanism = {signature; version; mechanism; as_server = false; filler}

    let init_state = START

    let handle (current_state, event) = 
    match (current_state , event) with
        | (START, Recv_sig(b)) -> 
            if (Bytes.get b 0) = (Char.chr 255) && (Bytes.get b 9) = (Char.chr 127) 
            then (let ng = new_greeting M.name |> to_bytes in
                
                Logs.info (fun f -> f "Greeting length: %d\n" (Bytes.length ng));
                (SIGNATURE, Send_bytes (ng)) 
                
                )
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
    | Close

    module Greeting = New_Greeting (M)
    
    type t = {
        mutable stage : stage;
        mutable as_server : bool;
        counter : int;
        greeting_state : Greeting.t;
        handshake_state : M.t;
        security_policy : string
    }

    let new_connection () = {
        stage = GREETING;
        as_server = false;
        counter = 0;
        greeting_state = Greeting.init_state;
        handshake_state = M.init_state;
        (* Set custom policy *)
        security_policy = M.name
    }

    let connection_fsm t bytes = 
        match (t.stage) with
            | GREETING ->   (let len = Bytes.length bytes in
                            let rec convert greeting_action_list =
                                match greeting_action_list with
                                    | [] -> []
                                    | (hd::tl) ->
                                        match hd with 
                                            | Greeting.Send_bytes(b) -> (Write(b)::(convert tl))
                                            | Greeting.Set_server(b) -> t.as_server <- b; convert tl
                                            (* Assume security mechanism is pre-set*)
                                            | Greeting.Check_mechanism(s) -> if s <> t.security_policy then 
                                                                                (Logs.info (fun f -> f "Security Policy mismatch: received (%s) with length %d but expecting (%s) with length %d\n" s (String.length s) t.security_policy (String.length t.security_policy));
                                                                                [Close])
                                                                             else convert tl
                                            | Greeting.Continue -> convert tl
                                            | Greeting.Ok -> Logs.info (fun f -> f "Greeting OK\n"); t.stage <- HANDSHAKE; convert tl
                                            | Greeting.Error(s) -> Logs.info (fun f -> f "Greeting FSM error: %s\n" s);
                                                                   [Close] in
                            (* Hard code the length here. The greeting is either complete or split into 11 + 53 or 10 + 54*)
                            match len with
                                (* Full greeting *)
                                | 64 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                            [Recv_sig(Bytes.sub bytes 0 10); 
                                             Recv_Vmajor(Bytes.sub bytes 10 1);
                                             Recv_Vminor(Bytes.sub bytes 11 1);
                                             Recv_Mechanism(Bytes.sub bytes 12 20);
                                             Recv_as_server(Bytes.sub bytes 32 1);
                                             Recv_filler  
                                            ]) [] in
                                        let connection_action = convert action_list in 
                                        ({t with greeting_state = state}, connection_action)
                                (* Signature + version major *)
                                | 11 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, [Recv_sig(Bytes.sub bytes 0 10); Recv_Vmajor(Bytes.sub bytes 10 1)]) [] in
                                        ({t with greeting_state = state}, convert action_list)
                                (* Signature *)
                                | 10 -> let (state, action) = Greeting.handle (t.greeting_state, Recv_sig(bytes)) in
                                        ({t with greeting_state = state}, convert [action])
                                (* version minor + rest *)
                                | 53 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                            [Recv_Vminor(Bytes.sub bytes 0 1);
                                             Recv_Mechanism(Bytes.sub bytes 1 20);
                                             Recv_as_server(Bytes.sub bytes 21 1);
                                             Recv_filler   
                                            ]) [] in
                                        let connection_action = convert action_list in 
                                        ({t with greeting_state = state}, connection_action)
                                (* version major + rest *)
                                | 54 -> let (state, action_list) = Greeting.handle_list (t.greeting_state, 
                                            [Recv_Vmajor(Bytes.sub bytes 0 1);
                                             Recv_Vminor(Bytes.sub bytes 1 1);
                                             Recv_Mechanism(Bytes.sub bytes 2 20);
                                             Recv_as_server(Bytes.sub bytes 22 1);
                                             Recv_filler   
                                            ]) [] in
                                        let connection_action = convert action_list in 
                                        ({t with greeting_state = state}, connection_action)
                                | _ ->  (t, [Close])
                        )
            | HANDSHAKE -> (let command = Command.of_frame (Frame.of_bytes bytes) in
                            let (new_state, action) = M.fsm t.handshake_state command in
                            let convert action = 
                                match action with 
                                    | M.Write(b) -> Write(b)
                                    | M.Continue -> Continue
                                    | M.Close -> Close
                            in
                            ({t with handshake_state = new_state}, [convert action]))
            | TRAFFIC -> (t, [])
            | ERROR -> (t, [])

end

module Connection_tcp (S: Mirage_stack_lwt.V4) (C: Connection) = struct
        let buffer_to_string data = 
        let content = ref [] in
            Bytes.iter (fun b -> content := Char.code b :: !content) data;
            String.concat " "  (List.map (fun x -> if (x >= 65 && x <= 90) || (x >= 97 && x <= 122) then String.make 1 (Char.chr x) else string_of_int x)  (List.rev !content))

        let connect s port =
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
                                            | C.Write(b) -> Logs.info (fun f -> f "Connection FSM Write %d bytes\n" (Bytes.length b));
                                                            (S.TCPV4.write flow (Cstruct.of_bytes b) >>= function
                                                            | Error _ -> (Logs.warn (fun f -> f "Error writing data to established connection."); Lwt.return_unit)
                                                            | Ok () -> act tl)
                                            | C.Continue -> Logs.info (fun f -> f "Connection FSM Continue\n");
                                                            act tl
                                            | C.Close -> Logs.info (fun f -> f "Connection FSM Close\n");
                                                         Lwt.return_unit))
                                in
                                    act action_list))
        in
            S.listen_tcpv4 s ~port (
                fun flow ->
                    let dst, dst_port = S.TCPV4.dst flow in
                    Logs.info (fun f -> f "new tcp connection from IP %s on port %d" (Ipaddr.V4.to_string dst) dst_port);
                    read_and_print flow (C.new_connection ())
            );
            S.listen s
end

module Context = struct
    type t = {options : int}
    let create_context () = {options = 0}
end

module type Socket = sig
    type t
    val create_socket : Context.t -> socket_type -> t
end

module Socket_tcp (S : Mirage_stack_lwt.V4) = struct
    type transport_info = Tcp of string * int
    type t = {
        socket_type : socket_type;
        mutable transport_info : transport_info;
        mutable security_mechanism : mechanism_type
    }
    let default_t = {
        socket_type = REP;
        transport_info = Tcp("", 0);
        security_mechanism = NULL
    }
    let create_socket context socket_type =
        match socket_type with
            | REP -> {default_t with socket_type = REP}
            | REQ -> {default_t with socket_type = REQ}
            | DEALER -> {default_t with socket_type = DEALER}
            | ROUTER -> {default_t with socket_type = ROUTER}
    
    let bind t port s = 
    let module C = 
        (val (match t.socket_type with
                | REP ->(match t.security_mechanism with
                            | NULL -> (module New_Connection (REP_socket) (NULL_mechanism) :Connection)
                            | PLAIN -> (module New_Connection (REP_socket) (PLAIN_mechanism) : Connection)
                        )
                | _ -> raise Not_Implemented) 
            : Connection) in
    let module C_tcp = Connection_tcp (S) (C) in
        C_tcp.connect s port

    let connect t = raise Not_Implemented
    let set_mechanism t mechanism = ()
end






module type Traffic = sig
    type t
end


