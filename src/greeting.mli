(** A greeting consists of ZMTP signature, version, mechanism and filler. *)
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