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
    | SIGNATURE
    | VERSION_MAJOR
    | VERSION_MINOR
    | MECHANISM
    | FILLER

type event =
    | Recv_bytes of bytes
    | Init of string

type action =
    | Send_bytes of bytes
    | Set_mechanism of string
    | Continue
    | Error of string

type t = {
    state          : state;
    match_start    : int;
    match_len      : int;
    bytes_to_match : bytes;
    mutable buffer : bytes;
}

let init_fsm = {
    state = SIGNATURE;
    match_start = 0;
    match_len = 10;
    bytes_to_match = signature;
    buffer = Bytes.empty
}

let place_holder = (init_fsm, [])

let handle t event = 
    match event with
        | Recv_bytes(b) -> 
            (match t.state with
                | SIGNATURE -> place_holder
                | VERSION_MAJOR -> place_holder
                | _ -> (init_fsm, []))
        | Init(mechanism) -> (init_fsm, [Send_bytes(to_bytes (new_greeting mechanism))])