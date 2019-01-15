open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context PUB in
            Socket.bind socket 5556 s;
            let rec publish content =
            try
                Socket.send socket [Data(content)];
                Logs.info (fun f -> f "work item sent");
                Lwt.pause () >>= fun () -> publish content
            with No_Available_Peers -> Lwt.pause () >>= fun () -> publish content
            in Logs.info (fun f -> f "Started socket\n"); publish "ABC"
end