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
    type state = 
    type action = Send
    let handle state command = (state, Send)
end