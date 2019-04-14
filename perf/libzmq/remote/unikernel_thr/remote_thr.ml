open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let message_size = 100 in
        let message = Bytes.to_string (Bytes.create message_size) in
        let message_count = 10000000 in
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context PUSH in
            Socket.connect socket "127.0.0.1" 5555 s >>= fun () ->
            let rec send_jobs n =
            if n <> 0 then
            try
                Socket.send socket (Data(message)) >>= fun () ->
                send_jobs (n - 1)
            with No_Available_Peers -> Lwt.pause () >>= fun () -> send_jobs n
            else Lwt.return_unit
            in send_jobs message_count
end
