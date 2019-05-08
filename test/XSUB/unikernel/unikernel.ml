open Lwt.Infix
open Mirage_zmq
module Main (S: Mirage_stack_lwt.V4) = struct
    let start s =
        let context = Context.create_context () in
        let module Socket = Socket_tcp (S) in
        let socket = Socket.create_socket context XSUB in
            Socket.connect socket "127.0.0.1" 5556 s >>= fun () ->
            let rec read_and_print () =
                Socket.recv socket >>= function
                | Data(msg) -> Logs.info (fun f -> f "Received msg: %s\n" msg);
                        Lwt.return_unit;
                | _ -> Logs.info (fun f -> f "Unexpected msg received\n");  Lwt.return_unit in
            Logs.info (fun f -> f "Started socket"); Socket.send socket (Data((String.make 1 (Char.chr 1)) ^ "ABC")) >>= fun () -> read_and_print (); 
            
end
