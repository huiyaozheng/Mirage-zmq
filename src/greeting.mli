(** A greeting consists of ZMTP signature, version, mechanism and filler. *)
type t

type event =
    | Recv_bytes of bytes
    | Init of string

type action =
    | Send_bytes of bytes
    | Set_mechanism of string
    | Continue
    | Error of string

val handle : t -> event -> t * action list

val handle_list : t -> event list -> t * action list
