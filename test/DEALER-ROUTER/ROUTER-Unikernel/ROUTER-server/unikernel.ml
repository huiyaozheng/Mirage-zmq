open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Mirage_zmq.Context.create_context () in
        let module Socket = Mirage_zmq.Socket_tcp (S) in
        let socket = Socket.create_socket context Mirage_zmq.REQ in
            (*Socket.set_identity socket "12345";*)
            Socket.connect socket "127.0.0.1" 5556 s >>= fun () ->
            let read_and_print : unit Lwt.t =
                Socket.send socket "World";
                Socket.recv socket >>= fun msg -> match msg with
                | "" -> Lwt.return_unit
                | _ -> Logs.info (fun f -> f "Received msg: %s\n" msg); 
                        Lwt.return_unit
            in  Logs.info (fun f -> f "Started socket \n"); 
                read_and_print
end
