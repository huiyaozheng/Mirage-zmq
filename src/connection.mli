type t

type action =
    | WRITE of bytes
    | CONTINUE
    | CLOSE

val new_connection : t

val connection_fsm : t -> bytes -> t * action list

module type Connection = sig
    type t
    type stage
    type action =
    | WRITE of bytes
    | CONTINUE
    | CLOSE
    val connection_fsm : t -> Bytes.t -> t * action list
    val new_connection : unit -> t
end