open Lwt.Infix
open Mirage_zmq
exception Internal_Error of string
module Main (S : Mirage_stack_lwt.V4) = struct
  let start s =
    let message_size = 100 in
    let round_trip_count = 100 in
    let context = Context.create_context () in
    let module Socket = Socket_tcp (S) in
    let socket = Socket.create_socket context REP in
    Socket.bind socket 5555 s ;
    let rec read_and_print n =
        if n = 0 then Lwt.return_unit else
      Socket.recv socket
      >>= fun msg ->
      match msg with
      | Data msg ->
          if String.length msg <> message_size then
            raise (Internal_Error "Message size not valid")
          else 
          Socket.send socket
            (Data msg)
          >>= fun () -> read_and_print (n-1)
      | _ ->
          raise (Internal_Error "Unexpected message received") Lwt.return_unit
    in
    read_and_print round_trip_count
end
