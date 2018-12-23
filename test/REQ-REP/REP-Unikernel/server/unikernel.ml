open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Mirage_zmq.Context.create_context () in
        let module Socket = Mirage_zmq.Socket_tcp (S) in
        let socket = Socket.create_socket context Mirage_zmq.REP in
            Socket.bind socket 5556 s;
            let read_and_print : unit Lwt.t =
                Socket.recv socket >>= fun msg -> match msg with
                | "" -> Lwt.return_unit
                | _ -> Logs.info (fun f -> f "Received msg: %s\n" msg); 
                        Socket.send socket "World";
                        Lwt.return_unit
            in  Logs.info (fun f -> f "Started socket listening\n"); 
                read_and_print
end
