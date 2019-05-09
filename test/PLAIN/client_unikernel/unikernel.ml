open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Mirage_zmq.Context.create_context () in
        let module Socket = Mirage_zmq.Socket_tcp (S) in
        let socket = Socket.create_socket context ~mechanism:PLAIN REQ in
            Socket.set_plain_credentials socket "admin" "password";
            Socket.connect socket "127.0.0.1" 5556 s >>= fun () ->
            let read_and_print : unit Lwt.t =
                Socket.send socket (Data("Hello")) >>= fun () ->
                Socket.recv socket >>= function
                | Data(msg) -> Logs.info (fun f -> f "Received msg: %s" msg); 
                        Lwt.return_unit
                | _ -> Lwt.return_unit
            in  Logs.info (fun f -> f "Started socket"); 
                read_and_print
end
