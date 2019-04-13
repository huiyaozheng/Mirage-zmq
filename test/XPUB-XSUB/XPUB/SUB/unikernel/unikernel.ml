open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context XPUB in
            Socket.set_outgoing_queue_size socket 10;
            Socket.bind socket 5556 s;
            let rec publish content =
            try
                Socket.send socket (Data(content)) >>= fun () ->
                (*Logs.info (fun f -> f "work item sent"); *)
                Lwt.pause () >>= fun () -> publish content
            with No_Available_Peers -> Lwt.pause () >>= fun () -> publish content
            in 
            let receive_subscription () = (
            Socket.recv socket >>= fun msg -> match msg with
                | Data(msg) -> Logs.info (fun f -> f "Received msg: %s\n" msg); 
                        Lwt.return_unit
                | _ -> Lwt.return_unit) in
            Logs.info (fun f -> f "Started socket\n"); Lwt.async receive_subscription; publish "ABC";
            
end
