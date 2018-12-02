type socket_type = REQ | REP | DEALER | ROUTER

type socket = {
    socket_type : socket_type
}

type context = {}

module type Socket = sig
    val name : string
    val make_socket : socket
end

module type Connection = sig
    type t
end

module Frame : sig
    type t
    val make_frame : bytes -> bool -> bool -> t
    val to_bytes : t -> bytes
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
end

module Command : sig
    type t
    val to_frame : t -> Frame.t
end = struct
    type t = { name : string; data : bytes }
    let to_frame t = Frame.make_frame (Bytes.concat Bytes.empty [Bytes.of_string t.name; t.data]) false true 
end

module type Security_Mechanism = sig
    type t
    val name : string
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
        | Set_mechanism of string
        | Set_server of bool
        | Continue
        | Ok
        | Error of string
    val handle : t * event -> t * action
    val handle_list : t * event list -> action list -> t * action list
end 

module New_Greeting (M : Security_Mechanism) : Greeting = struct
    type t =
        | START
        | SIGNATURE
        | VERSION_MAJOR
        | VERSION_MINOR
        | MECHANISM
        | AS_SERVER
        | FILLER
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
        | Set_mechanism of string
        | Set_server of bool
        | Continue
        | Ok
        | Error of string
    type version = { major : bytes; minor : bytes}

    type greeting = { 
        signature  : bytes;
        version    : version;
        mechanism  : string;
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

    let to_bytes g = 
        Bytes.concat Bytes.empty [g.signature; g.version.major; g.version.minor; pad_mechanism g.mechanism; g.filler]

    let new_greeting mechanism = {signature; version; mechanism; filler}

    let handle (current_state, event) = 
    match (current_state , event) with
        | (START, Recv_sig(b)) -> 
            if (Bytes.get b 0) = (Char.chr 255) && (Bytes.get b 7) = (Char.chr 127) 
            then (SIGNATURE, Send_bytes (new_greeting M.name |> to_bytes)) 
            else (ERROR, Error("Protocol Signature not detected."))
        | (SIGNATURE, Recv_Vmajor(b)) ->
            if (Bytes.get b 0) = (Char.chr 3) 
            then (VERSION_MAJOR, Continue) 
            else (ERROR, Error("Version-major is not 3."))
        | (VERSION_MINOR, Recv_Vmajor(b)) ->
            if (Bytes.get b 0) = (Char.chr 0) 
            then (MECHANISM, Continue) 
            else (ERROR, Error("Version-minor is not 0."))
        | (MECHANISM, Recv_Mechanism(b)) ->
            (AS_SERVER, Set_mechanism(b))
        | (AS_SERVER, Recv_as_server(b)) ->
            if (Bytes.get b 0) = (Char.chr 0)
            then (FILLER, Set_server(false))
            else (FILLER, Set_server(true))
        | (FILLER, Recv_filler) -> (SUCCESS, Ok)
        | _ -> (ERROR, Error("Unexpected event."))

    let rec handle_list (current_state, event_list) action_list =
    match event_list with
        | [] -> (match current_state with 
                    | ERROR -> (ERROR, [List.hd action_list])
                    | _ -> (current_state, List.rev action_list))
        | hd::tl ->
        match current_state with
            | ERROR -> (ERROR, [List.hd action_list])
            | _ -> let (new_state, action) = handle (current_state, hd) in
                handle_list (new_state, tl) (action::action_list)
end

module type Traffic = sig
    type t
end

module REP : Socket = struct
    let name = "REP"
    let make_socket = {socket_type = REP}
end

let new_context = context

let create_socket context socket_type =
    match socket_type with
        | REP -> let new_socket = REP.make_socket
        | REQ ->
        | ROUTER ->
        | DEALER ->
