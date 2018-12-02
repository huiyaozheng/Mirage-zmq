type t

module type Connection = sig
    type t
    type stage
    type action =
    | Write of bytes
    | Continue
    | Close
    val connection_fsm : t -> Bytes.t -> t * action list
    val new_connection : unit -> t
end