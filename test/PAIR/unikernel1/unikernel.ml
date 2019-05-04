open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context PAIR in
            Socket.bind socket 5556 s;
            let rec send content =
            try
                Socket.send socket (Data(content)) >>= fun () ->
                Logs.info (fun f -> f "Greeting sent");
                Lwt.return_unit
            with No_Available_Peers -> Lwt.pause () >>= fun () -> send content
            in let recv () =
                Socket.recv socket >>= function
                | Data(msg) -> Logs.info (fun f -> f "Received msg: %s\n" msg); Lwt.return_unit
                | _ -> Logs.info (fun f -> f "Unexpected msg received\n"); Lwt.return_unit
            in Logs.info (fun f -> f "Started socket\n"); send "Hi there" >>= fun () -> recv ();
end
