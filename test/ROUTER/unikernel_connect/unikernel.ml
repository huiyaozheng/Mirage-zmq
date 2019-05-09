open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context ROUTER in
            Socket.connect socket "127.0.0.1" 5556 s >>= fun () ->
            let rec read_and_print () =
                Socket.recv socket >>= function
                | Identity_and_data(id, msg) -> Logs.info (fun f -> f "Received msg: %s" msg); Socket.send socket (Identity_and_data(id, "Received")) 
                | _ -> Lwt.return_unit
            in  Logs.info (fun f -> f "Started socket"); 
                Socket.send socket (Identity_and_data("router", "Hello")) >>= fun () -> read_and_print()
end
