type stage = 
    | GREETING
    | HANDSHAKE
    | TRAFFIC

let stage = GREETING

let recv bytes = 
    match stage with
        | GREETING -> ()

        | HANDSHAKE -> ()
        | TRAFFIC ->()

module type t = sig
    val recv : Bytes.t -> unit
end

module Create_connection (C : t) = struct
    let state = GREETING
    let counter = 0
end