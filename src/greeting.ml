type version = {
    major : bytes;
    minor : bytes
}

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

(** TODO: check validity of the greeting*)
let decode_greeting byte_sequence = 
    let mechanism = Bytes.sub_string byte_sequence 12 20 in 
        {signature; version; mechanism; filler}


type state =
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

let handle (current_state, event) = 
    match (current_state , event) with
        | (START, Recv_sig(b)) -> 
            if (Bytes.get b 0) = (Char.chr 255) && (Bytes.get b 7) = (Char.chr 127) 
            then (SIGNATURE, Continue) 
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

let handle_list (current_state, event_list) action_list =
    match event_list with
        | [] -> (match current_state with 
                    | ERROR -> (ERROR, [List.hd action_list])
                    | _ -> (current_state, List.rev action_list))
        | hd::tl ->
        match current_state with
            | ERROR -> (ERROR, [List.hd action_list])
            | _ -> let (new_state, action) = handle (current_state, hd) in
                handle_list (new_state, tl) (action::action_list)