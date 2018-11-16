(** A greeting consists of ZMTP signature, version, mechanism and filler. *)
type greeting

(** Given a mechanism, return a new greeting. *)
val new_greeting : string -> greeting 

(** Converts a greeting to a sequence of bytes*)
val to_bytes : greeting -> bytes

(** Decodes a greeting byte sequence into a greeting. *)
val decode_greeting : bytes -> greeting