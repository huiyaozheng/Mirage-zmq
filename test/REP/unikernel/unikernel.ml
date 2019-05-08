open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context REP in
            Socket.bind socket 5556 s;
            let rec read_and_print () =
                Socket.recv socket >>= function
                | Data(msg) -> Logs.info (fun f -> f "Received msg: %s" msg); 
                        Socket.send socket (Data("Received")) >>= fun () ->
                        read_and_print ();
                | _ -> Logs.info (fun f -> f "Unexpected msg received\n");  Lwt.return_unit
            in  Logs.info (fun f -> f "Started socket"); read_and_print ()
end
