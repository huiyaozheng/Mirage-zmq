open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start (s : S.t) =
        let context = Mirage_zmq.Context.create_context () in
        let module Socket = Mirage_zmq.Socket_tcp (S) in
        let socket = Socket.create_socket context REQ in
            Socket.connect socket "127.0.0.1" 5556 s >>= fun () ->
            let rec read_and_print n =
                if n < 1 then Lwt.return_unit else
                Socket.send socket (Data("Hello")) >>= fun () ->
                Socket.recv socket >>= function
                | Data(msg) -> Logs.info (fun f -> f "Received msg: %s" msg); 
                        read_and_print (n - 1)
                | _ -> Lwt.return_unit
            in  Logs.info (fun f -> f "Started socket"); 
                read_and_print 10
end
