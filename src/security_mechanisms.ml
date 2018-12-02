module type Mechanism = sig
    val name : string
    type state
    type action
    val handle : state -> Command.t -> state * action
end

type property = string * string

type metadata = List of property

(*let ready metadata =
    Command.({name = Bytes.concat Bytes.empty [Char.chr 213; "READY"]; data = metadata}) *)


module Null_mechanism : Mechanism = struct
    let name = "NULL"
    type state = OK | READY | ONGOING | CLOSE
    type action = Send of bytes | Continue

    let error reason = Frame.make_frame (Bytes.concat (Bytes.empty) [Bytes.make 1 (Char.chr 213); Bytes.of_string "ERROR"; Bytes.of_string reason]) false true

    let ready = (Char.escaped (Char.chr 5)) ^ "READY"
    
    let handle state (command : Command.t) = 
        match state with 
            | READY -> (
                match command.name with
                    | "READY" -> (OK, Continue)
                    | _ -> (CLOSE, Send(Frame.to_bytes (error "unexpected command."))))
            | ONGOING -> (OK, Continue)
            | _ -> (OK, Continue)
end