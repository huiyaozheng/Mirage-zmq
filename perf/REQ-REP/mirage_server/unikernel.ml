open Lwt.Infix
open Mirage_zmq

module Main (S : Mirage_stack_lwt.V4) = struct
  let start s =
    let context = Context.create_context () in
    let module Socket = Socket_tcp (S) in
    let socket = Socket.create_socket context REP in
    Socket.bind socket 5555 s ;
    let rec read_and_print () =
      Socket.recv socket
      >>= fun msg ->
      match msg with
      | Data msg ->
          Logs.info (fun f -> f "Received msg: %s\n" msg) ;
          Socket.send socket
            (Data
               "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          >>= fun () -> read_and_print ()
      | _ ->
          Lwt.return_unit
    in
    Logs.info (fun f -> f "Started socket\n") ;
    read_and_print ()
end
