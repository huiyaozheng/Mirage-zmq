type t

type action =
    | WRITE of bytes
    | CONTINUE
    | CLOSE

val new_connection : t

val connection_fsm : t -> bytes -> t * action list