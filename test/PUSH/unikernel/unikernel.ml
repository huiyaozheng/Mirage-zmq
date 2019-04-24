open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context PUSH in
            Socket.bind socket 5556 s;
            let rec send_jobs n =
            try
                Socket.send socket (Data("Work item " ^ (string_of_int n))) >>= fun () ->
                Logs.info (fun f -> f "work item sent");
                send_jobs (n + 1)
            with No_Available_Peers -> Lwt.pause () >>= fun () -> send_jobs n
            in Logs.info (fun f -> f "Started socket\n"); send_jobs 1
end
